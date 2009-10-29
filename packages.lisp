(cl:defpackage "URI-TEMPLATE"
  (:use "COMMON-LISP" "CL-PPCRE")
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

(cl:defpackage "URI-TEMPLATE.TEST"
  (:use "COMMON-LISP" "URI-TEMPLATE")
  (:export
   #:run-tests))
