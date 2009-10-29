;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

(asdf:defsystem :uri-template
  :serial t
  :version "0.5"
  :components ((:file "packages")
               (:file "uri-template")
               (:file "destructure-uri"))
  :depends-on (:kmrcl :cl-ppcre))

(asdf:defsystem :uri-template.test
  :serial t
  :components ((:file "packages")
               (:file "uri-template-test"))
  :depends-on (:uri-template))
