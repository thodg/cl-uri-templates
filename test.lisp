;;  cl-uri-templates
;;  Extensive URI-Templates implementation in Common-Lisp.
;;
;;  Copyright 2009 Thomas de Grivel <billitch@gmail.com>
;;  Copyright (c) 2007, 2008, 2009 Vladimir Sedach
;;
;;  This software is provided "AS IS".
;;  Please see COPYING for details.

(require 'asdf)
(require 'cl-uri-templates)
(require 'FiveAM)

(in-package #:cl-user)

(defpackage #:cl-uri-templates.test
  (:use #:common-lisp #:cl-uri-templates #:FiveAM)
  (:export #:run-tests
           #:run-interpolation-tests
           #:run-destructuring-tests))

(in-package #:cl-uri-templates.test)


(defmacro define-fixture (name args &body body)
  `(handler-case
       (def-fixture ,name ,args ,@body)
     (warning nil)))


(def-suite disabled-tests)
(def-suite enabled-tests)


(def-suite expansions :in enabled-tests)
(in-suite expansions)


(define-fixture uri-template-syntax ()
  (let ((*readtable* (copy-readtable nil)))
    (enable-uri-template-syntax)
    (&body)))


(defmacro eval-read (string)
  `(eval (read-from-string ,string)))


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
    (signals (invalid-expansion-error) (eval-read "#U{-opt|b|c=,}"))
    (signals (invalid-expansion-error) (eval-read "#U{%"))))


(test (uri-warnings :suite disabled-tests)
  (with-fixture uri-template-syntax ()
    (signals (invalid-uri-warning) (eval-read "#Uaa}"))
    (signals (invalid-uri-warning) (eval-read "#U<>"))
    (signals (invalid-uri-warning) (eval-read "#U%"))
    (signals (invalid-uri-warning) (eval-read "#U%1"))
    (signals (invalid-uri-warning) (eval-read "#U%1g"))
    (signals (invalid-uri-warning) (eval-read "#U%a"))
    (signals (invalid-uri-warning) (eval-read "#U%ag"))
    (signals (invalid-uri-warning) (eval-read "#U%ga"))
    (signals (invalid-uri-warning) (eval-read "#U%gg"))))


(def-suite variables :in expansions)
(in-suite expansions)


(define-fixture some-variables ()
  (let ((baz 1)
        (bar "bar"))
    (declare (ignorable bar baz))
    (&body)))


(test invalid-variables
  (signals invalid-var-error (eval '(expand-uri-template "{}")))
  (signals invalid-var-error (eval '(expand-uri-template "{@}")))
  (signals invalid-var-error (eval '(expand-uri-template "{{}")))
  (signals invalid-var-error (eval '(expand-uri-template "{ }")))
  (signals invalid-var-error (eval '(expand-uri-template "{a@}")))
  (signals invalid-var-error (eval '(expand-uri-template "{a{}")))
  (signals invalid-var-error (eval '(expand-uri-template "{a }")))
  (signals invalid-var-error (eval '(expand-uri-template "{@a}")))
  (signals invalid-var-error (eval '(expand-uri-template "{{a}")))
  (signals invalid-var-error (eval '(expand-uri-template "{ a}")))
  (signals invalid-var-error (eval '(expand-uri-template "{a@a}")))
  (signals invalid-var-error (eval '(expand-uri-template "{a{a}")))
  (signals invalid-var-error (eval '(expand-uri-template "{a a}")))
  (signals invalid-var-error (eval '(expand-uri-template "a{a@a}a")))
  (signals invalid-var-error (eval '(expand-uri-template "a{a{a}a")))
  (signals invalid-var-error (eval '(expand-uri-template "a{a a}a")))
  (signals invalid-var-error (eval '(expand-uri-template "{=}")))
  (signals invalid-var-error (eval '(expand-uri-template "{={}")))
  (signals invalid-var-error (eval '(expand-uri-template "{=a}")))
  (signals invalid-var-error (eval '(expand-uri-template "{|}")))
  (signals invalid-var-error (eval '(expand-uri-template "{a={}")))
  (signals invalid-var-error (eval '(expand-uri-template "{.abc}")))
  (signals invalid-var-error (eval '(expand-uri-template "{+4abc}")))
  (signals invalid-var-error (eval '(expand-uri-template "{4a:bc}")))
  (signals invalid-var-error (eval '(expand-uri-template "{abc}{_abc}"))))


(test variable-substitution
  "Successful variable substitutions"
  (is (string= "fredfredfred"
               (let ((foo "fred"))
                 (expand-uri-template "{foo}{foo=}{foo=wilma}"))))
  (is (string= "wilma"
               (expand-uri-template "{bar=wilma}")))
  (is (string= ""
               (expand-uri-template "{baz}{1baz}{123}{1-2}{1-baz}{ba.z_1-2}")))
  (is (string= "foo..baz.qux."
               (let ((f.oo_ "foo")
                     (b-a-r nil)
                     (b-a.z_ "baz"))
                 (expand-uri-template "{f.oo_}.{b-a-r}.{b-a.z_=nn}.{4q.-_x=qux}."))))
  (is (string= "http://example.org/?q=fred"
               (let ((bar "fred"))
                 (expand-uri-template "http://example.org/?q={bar}"))))
  (is (string= "http://www.foo.com/bar/1"
               (let ((baz 1))
                 (expand-uri-template "http://www.foo.com/bar/{baz}"))))
  (is (string= "http://www.foo.com/bar/bar/1"
               (let ((bar "bar")
                     (baz 1))
                 (expand-uri-template "http://www.foo.com/bar/{bar}/{baz}"))))
  (is (string= "http://www.foo.com/bar/bar1"
               (let ((bar "bar")
                     (baz 1))
                 (expand-uri-template "http://www.foo.com/bar/{bar}{baz}"))))
  (with-fixture uri-template-syntax ()
    (is (string= "...1.wil.."
                 (eval-read "(let (foo bar (baz 1) (qux \"wil\"))
                               #U.{foo}.{bar=wilma}.{baz}.{qux=wilma}.{flub}.)")))))


(def-suite operators :in expansions)
(in-suite operators)


(test operator-opt
  "Operator -opt"
  (is (string= ""
               (expand-uri-template "{-opt||foo}{-opt|bar|foo}")))
  (is (string= "..bar."
               (let (foo (bar 1))
                 (expand-uri-template ".{-opt||foo}.{-opt|bar|bar}."))))
  (is (string= ".foo.bar."
               (let ((foo ""))
                 (expand-uri-template ".{-opt|foo|foo}.{-opt|bar|bar=1}."))))
  (signals invalid-op-vars-error (parse-uri-template "{-opt||foo,bar}"))
  (with-fixture uri-template-syntax ()
    (is (string= "foo"
                 (eval-read "(let ((foo 1))
                               #U.{-opt|foo|foo})")))))


(test operator-neg
  "Operator -neg"
  (is (string= "..bar."
               (expand-uri-template ".{-neg||foo}.{-neg|bar|foo}.")))
  (is (string= ".foo.."
               (expand-uri-template ".{-neg|foo|foo}.{-neg|bar|bar=}.")))
  (is (string= "..."
               (let (foo (bar 1))
                 (expand-uri-template ".{-neg||foo}.{-neg|bar|bar}."))))
  (is (string= "..."
               (let ((foo ""))
                 (expand-uri-template ".{-neg|foo|foo}.{-neg|bar|bar=1}."))))
  (signals invalid-op-vars-error (parse-uri-template "{-neg||foo,bar}"))
  (with-fixture uri-template-syntax ()
    (is (string= "..bar."
                 (eval-read "(let ((foo 1))
                               #U.{-neg|foo|foo}.{-neg|bar|bar})")))))


(with-open-file (*standard-output* "test.output" :direction :output
                                   :if-exists :supersede)
  (time (run! 'enabled-tests)))
