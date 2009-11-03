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


(define-condition expansion-error (reader-error)
  ((last-arg :initarg :last-arg :reader last-arg-of :initform nil))
  (:report (lambda (condition stream)
             (format stream "Invalid URI expansion near ~A."
                     (last-arg-of condition)))))


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
                   `((assert ,(char-predicate valid-next-char next-char)
                             () "Next char ~A is invalid after ~A."
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


(defconstant +uri-reserved-chars+ ";/?:@&=+$,")


(defun uri-reserved-char-p (char)
  (find char +uri-reserved-chars+))


(defconstant +uri-mark-chars+ "-_.!~*'()")


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
                    (assert (and (> (length result) 0)
                                 (alphanumericp (char result 0)))
                            () "Invalid variable name : ~A. ~
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
  (let ((name (intern (string-upcase (read-varname stream)))))
    (if (eat-char stream #\=)
        (cons name (read-vardefault stream))
        name)))


(defun read-vars (stream)
  (declare (type stream stream))
  (loop
     for var = (read-var stream)
     for next-char = (peek-char nil stream)
     collect var
     while (char= #\, next-char)
     finally (assert (char= #\} (read-char stream))
               () "Invalid URI expansion vars near ~A" var)))


(defun intern-op (name)
  (let ((-name (concatenate 'string "-" (string-upcase name))))
    (assert (find-symbol -name 'cl-uri-templates.operators)
            () "~A is not a valid operator for URI expansion" -name)
    (intern -name 'cl-uri-templates.operators)))


(defun read-operator (stream)
  (declare (type stream stream))
  (assert (char= #\- (read-char stream)))
  (append (list (intern-op (read-op stream))
                (read-arg stream))
          (read-vars stream)))

(untrace read-expansion read-operator read-vars read-var read-vardefault read-varname read-arg read-op char=)

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
              (t (push next-char string-accumulator)))
         finally
           (unread-char next-char stream)
           (collect-string))
      (reverse token-accumulator))))


(defun parse-uri-template (str)
  (declare (type string str))
  (with-input-from-string (stream str)
    (read-uri-template stream)))


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
