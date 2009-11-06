;;  cl-uri-templates
;;  Extensive URI-Templates implementation in Common-Lisp.
;;
;;  Copyright 2009 Thomas de Grivel <billitch@gmail.com>
;;  Copyright (c) 2007, 2008, 2009 Vladimir Sedach
;;
;;  This software is provided "AS IS".
;;  Please see COPYING for details.

(in-package #:cl-uri-templates)


(defvar *encode-uri-string* t)

(declaim (optimize (debug 3)))


(define-condition invalid-uri-warning (warning)
  ((message :initarg :message
            :initform "Invalid URI-Template"
            :reader message-of))
  (:report (lambda (condition stream)
             (write-string (message-of condition) stream))))


(define-condition invalid-uri-error (reader-error)
  ((message :initarg :message
            :initform "Invalid URI-Template"
            :reader message-of))
  (:report (lambda (condition stream)
             (write-string (message-of condition) stream))))


(defmacro check-uri (test condition fmt &rest arguments)
  `(assert ,test () ',condition :message (format nil ,fmt ,@arguments)))


(defmacro define-reader (name &key allowed-char valid-next-char eat-next-char
                         valid-result)
  "Reads string from STREAM of characters satisfying ALLOWED-CHAR-SPEC.
   Next char in stream will satisfy VALID-NEXT-CHAR-SPEC and
   is eaten when EAT-NEXT-CHAR is non-nil.
   The *-CHAR-SPEC can be :
    - a string of allowed characters
    - a single allowed character
    - a symbol which functional value will be used as a predicate
    - a lambda expression to be used as a predicate"
  (assert allowed-char () "You must supply valid characters with :ALLOWED.")
  (flet ((char-predicate (spec char-var)
           (typecase spec
             (symbol `(,spec ,char-var))
             (string `(find ,char-var ,spec))
             (character `(char= ,char-var ,spec))
             (t `(funcall ,spec ,char-var)))))
    (let ((stream (gensym "STREAM"))
          (next-char (gensym "NEXT-CHAR"))
          (result (gensym "RESULT"))
          (char (gensym "CHAR")))
      `(defun ,name (,stream)
         (declare (type stream ,stream))
         (let* ((,next-char nil)
                (,result (coerce (loop
                                    for ,char = (read-char ,stream)
                                    while ,(char-predicate allowed-char char)
                                    collect ,char
                                    finally
                                      (setf ,next-char ,char)
                                      ,@(unless eat-next-char
                                                `((unread-char ,char ,stream))))
                                 'string)))
           ,@(when valid-next-char
                   `((check-uri ,(char-predicate valid-next-char next-char)
                                invalid-uri-error
                                "Next char ~S is invalid after ~S."
                                ,next-char ,result)))
           ,@(when valid-result
                   `((funcall ,valid-result ,result)))
           ,result)))))


(define-reader read-op
    :allowed-char alpha-char-p
    :valid-next-char #\|
    :valid-result (lambda (result)
                    (> (length result) 0))
    :eat-next-char t)


(defmacro define-constant (name value &key (test 'equal))
  (unless (and (constantp name)
               (funcall test value (symbol-value name)))
    `(eval-when (:compile-toplevel)
       (defconstant ,name ,value))))


(define-constant +uri-reserved-chars+ ";/?:@&=+$,")


(defun uri-reserved-char-p (char)
  (find char +uri-reserved-chars+))


(define-constant +uri-mark-chars+ "-_.!~*'()")


(defun uri-unreserved-char-p (char)
  (declare (type character char))
  (or (alphanumericp char)
      (find char +uri-mark-chars+)))


(define-reader read-arg
    :allowed-char (lambda (char)
                    (declare (type character char))
                    (or (uri-unreserved-char-p char)
                        (uri-reserved-char-p char)
                        (char= char #\%)))
    :valid-next-char #\|
    :eat-next-char t)


(define-reader read-varname
    :allowed-char (lambda (char)
                    (declare (type character char))
                    (or (alphanumericp char)
                        (find char "._-")))
    :valid-next-char "=,}"
    :valid-result (lambda (result)
                    (check-uri (and (> (length result) 0)
                                       (alphanumericp (char result 0)))
                            invalid-uri-error
                            "Invalid variable name : ~S. ~
                             Variable names must start with alphanum."
                            result)))


(define-reader read-vardefault
    :allowed-char (lambda (char)
                    (declare (type character char))
                    (or (uri-unreserved-char-p char)
                        (char= char #\%)))
    :valid-next-char ",}")


(defun eat-char (stream char)
  (when (char= char (peek-char nil stream))
    (read-char stream)))
      

(defun read-var (stream)
  (declare (type stream stream))
  (let* ((name (intern (string-upcase (read-varname stream)))))
    `(or (ignore-errors ,name)
         ,(if (eat-char stream #\=)
              (read-vardefault stream)
              ""))))


(defun read-vars (stream)
  (declare (type stream stream))
  (loop
     for var = (read-var stream)
     for next-char = (peek-char nil stream)
     collect var
     while (char= #\, next-char)
     finally (check-uri (char= #\} (read-char stream))
                        invalid-uri-error
                        "Invalid URI expansion vars near ~S"
                        var)))


(defun intern-op (name)
  (let ((-name (concatenate 'string "-" (string-upcase name))))
    (check-uri (find-symbol -name 'cl-uri-templates.operators)
               invalid-uri-error
               "~A is not a valid operator for URI expansion"
               -name)
    (intern -name 'cl-uri-templates.operators)))


(defun read-operator (stream)
  (declare (type stream stream))
  (check-uri (char= #\- (read-char stream))
             invalid-uri-error "operator must start with '-'.")
  (append (list (intern-op (read-op stream))
                (read-arg stream))
          (read-vars stream)))


(defun read-expansion (stream)
  (declare (type stream stream))
  (if (char= #\- (peek-char nil stream))
      (read-operator stream)
      (read-var stream)))


(defun read-uri-template (stream &optional recursive-p)
  (declare (type stream stream))
  (let ((*readtable* (copy-readtable nil))
        (token-accumulator ())
        (string-accumulator ()))
    (flet ((collect-string ()
             (when string-accumulator
               (push (coerce (reverse string-accumulator) 'string)
                     token-accumulator)
               (setf string-accumulator ()))))
      (loop
         for next-char = (read-char stream nil #\Space recursive-p)
         until (member next-char '(#\Space \#Tab #\Newline #\)))
         do (case next-char  
              (#\{ (collect-string)
                   (push (read-expansion stream)
                         token-accumulator))
              (#\})
              (t (push next-char string-accumulator)))
         finally
           (unread-char next-char stream)
           (collect-string))
      (reverse token-accumulator))))


(defmacro parse-uri-template (str)
  (declare (type string str))
  (with-input-from-string (stream str)
    (cons 'cl-uri-templates:uri-template
          (read-uri-template stream))))


(defun maybe-uri-encode (x)
  (if *encode-uri-string* (kmrcl:encode-uri-string (princ-to-string x)) x))


#+parenscript (parenscript:defpsmacro maybe-uri-encode (x)
                (if *encode-uri-string* `(encode-u-r-i-component ,x) x))


(defun uri-template (&rest template-args)
  (format nil "~{~A~}" template-args))


#+parenscript (parenscript:defpsmacro uri-template (&rest template-args)
                `(+ ,@template-args))


(defun enable-uri-template-syntax ()
  (set-dispatch-macro-character #\# #\U
    (lambda (stream subchar arg)
      (declare (ignore subchar arg))
      `(uri-template ,@(read-uri-template stream t))))
  (values))
