;;  cl-uri-templates
;;  Extensive URI-Templates implementation in Common-Lisp.
;;
;;  Copyright 2009 Thomas de Grivel <billitch@gmail.com>
;;  Copyright (c) 2007, 2008, 2009 Vladimir Sedach
;;
;;  This software is provided "AS IS".
;;  Please see COPYING for details.

(in-package #:cl-user)

(defpackage #:cl-uri-templates
  (:use #:common-lisp #:cl-ppcre)
  (:export
   ;; common
   #:uri-template

   ;; interpolation
   #:enable-uri-template-syntax
   #:read-uri-template
   #:*encode-uri-string*
   
   ;; destructuring
   #:*decode-uri-string*
   #:uri-template-bind
   #:bind-standard-uri-components

   #:%uri-scheme
   #:%uri-authority
   #:%uri-path
   #:%uri-query
   #:%uri-fragment
   #:%uri-head
   #:%uri-tail
   #:%uri-user
   #:%uri-host
   #:%uri-port
   #:%uri-directory
   #:%uri-file))
