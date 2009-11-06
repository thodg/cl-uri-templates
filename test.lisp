;;  cl-uri-templates
;;  Extensive URI-Templates implementation in Common-Lisp.
;;
;;  Copyright 2009 Thomas de Grivel <billitch@gmail.com>
;;  Copyright (c) 2007, 2008, 2009 Vladimir Sedach
;;
;;  This software is provided "AS IS".
;;  Please see COPYING for details.

(in-package #:cl-user)

(defpackage #:cl-uri-templates.test
  (:use #:common-lisp #:cl-uri-templates #:FiveAM)
  (:export #:run-tests
           #:run-interpolation-tests
           #:run-destructuring-tests))

(in-package #:cl-uri-templates.test)


(defmacro define-fixture (name args &body body)
  `(eval-when (:compile-toplevel)
     (handler-case
         (def-fixture ,name ,args ,@body)
       (warning nil))))


(def-suite all-tests)


(def-suite expansion-tests
    :description "Test URI-Templates expansions."
    :in all-tests)


(in-suite expansion-tests)


(define-fixture uri-template-syntax ()
  (let ((*readtable* (copy-readtable nil)))
    (enable-uri-template-syntax)
    (&body)))


(defun eval-read (string)
  (eval (read-from-string string)))


(test read-macro
  (with-fixture uri-template-syntax ()
    (is-true (read-from-string "#U"))
    (is (string= "" (eval-read "#U")))
    (is (string= "/" (eval-read "#U/")))
    (is (string= "" (eval-read "#U ")))
    (is (string= "a" (eval-read "#Ua ")))
    (is (string= "abc" (eval-read "#Uabc ")))
    (is (string= "a/b:c" (eval-read "#Ua/b:c")))
    (is (string= "%12" (eval-read "#U%12")))
    (is (string= "a%12/b:c" (eval-read "#Ua%12/b:c")))
    (signals (end-of-file) (eval-read "#U{"))
    (signals (end-of-file) (eval-read "#U{a"))
    (signals (end-of-file) (eval-read "#U{-"))
    (signals (end-of-file) (eval-read "#U{-a"))
    (signals (reader-error) (eval-read "#U<>"))
    (signals (invalid-uri-error) (eval-read "#U{-a|b|c=,}"))
    (signals (invalid-uri-error) (eval-read "#U{%"))
    (signals (invalid-uri-warning) (eval-read "#Uaa}"))
    (signals (invalid-uri-warning) (eval-read "#U<>"))
    (signals (invalid-uri-warning) (eval-read "#U%"))
    (signals (invalid-uri-warning) (eval-read "#U%1"))
    (signals (invalid-uri-warning) (eval-read "#U%1g"))
    (signals (invalid-uri-warning) (eval-read "#U%a"))
    (signals (invalid-uri-warning) (eval-read "#U%ag"))
    (signals (invalid-uri-warning) (eval-read "#U%ga"))
    (signals (invalid-uri-warning) (eval-read "#U%gg"))))


(define-fixture some-variables ()
  (let ((baz 1)
        (bar "bar"))
    (declare (ignorable bar baz))
    (&body)))


(test variable-expansion
  "Test variable expansion"
  (is (string= "http://www.foo.com/bar/1"
               (let ((baz 1))
                 (parse-uri-template "http://www.foo.com/bar/{baz}"))))
  (is (string= "http://www.foo.com/bar/bar/1"
               (let ((bar "bar")
                     (baz 1))
                 (parse-uri-template "http://www.foo.com/bar/{bar}/{baz}"))))
  (is (string= "http://www.foo.com/bar/bar1"
               (let ((bar "bar")
                     (baz 1))
                 (parse-uri-template "http://www.foo.com/bar/{bar}{baz}"))))
  (with-fixture uri-template-syntax ()
    (is (string= "http://www.foo.com/bar/1"
                 (eval-read "(let ((baz 1))
                                 #Uhttp://www.foo.com/bar/{baz})")))))


(with-open-file (*standard-output* "test.output" :direction :output
                                   :if-exists :supersede)
  (time (run! 'all-tests)))
