;;  cl-uri-templates
;;  Extensive URI-Templates implementation in Common-Lisp.
;;
;;  Copyright 2009 Thomas de Grivel <billitch@gmail.com>
;;  Copyright (c) 2007, 2008, 2009 Vladimir Sedach
;;
;;  This software is provided "AS IS".
;;  Please see COPYING for details.

(in-package #:cl-uri-templates)


(defvar *arity-p-of* (make-hash-table :test 'eq))


(defmacro lambda-list-arity-p (lambda-list)
  (loop
     with op = 'eql
     with op-text = "equal to"
     with count = 0
     for token in lambda-list
     until (when (eq token '&rest)
             (setf op '>=
                   op-text "greater than"))
     do (incf count)
     finally (return `(list (function (lambda (n)
                              (,op n ,count)))
                            ,(format nil "~A ~A" op-text count)))))


(defun check-op-arity (op arg-count)
  (let ((found (gethash op *arity-p-of*)))
    (when found
      (destructuring-bind (arity-p text) found
          (unless (funcall arity-p arg-count)
            (error 'invalid-op-vars-error
                   :message (format nil
                                    "Invalid variable count. The number of ~
                                     variables for operator ~A must be ~A."
                                    op text)))))))


(defmacro define-operator (name (arg &rest arguments) &body body)
  `(progn
     (setf (gethash ',name *arity-p-of*)
           (lambda-list-arity-p ,arguments))
     (defun ,name ,(cons arg arguments)
       ,@body)))


(in-package :cl-uri-templates.operators)


(define-operator -opt (arg var)
  (declare (type string arg))
  (if var
      arg
      ""))


(define-operator -neg (arg var)
  (declare (type string arg))
  (if var
      ""
      arg))


(define-operator -prefix (arg var)
  (if (consp var)
      (apply #'concatenate 'string (loop
                                      for v in var
                                      collect arg
                                      collect (princ-to-string (or v ""))))
      (concatenate 'string arg (princ-to-string (or var "")))))


(define-operator -suffix (arg var)
  (if (consp var)
      (apply #'concatenate 'string (loop
                                      for v in var
                                      collect (princ-to-string (or v ""))
                                      collect arg))
      (concatenate 'string (princ-to-string (or var "")) arg)))


(define-operator -join (arg &rest vars)
  (apply #'concatenate 'string (loop
                                  for v in vars
                                  for sep = "" then arg
                                  collect sep
                                  collect (princ-to-string (or v "")))))


(define-operator -list (arg var)
  (assert (typep var 'list)
          () 'cl-uri-templates:invalid-op-vars-error
          "Operator -list only accepts a variable containing a list.")
  (apply #'concatenate 'string (loop
                                  for v in var
                                  for sep = "" then arg
                                  collect sep
                                  collect (princ-to-string (or v "")))))
