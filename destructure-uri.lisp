(in-package "URI-TEMPLATE")

(defvar *decode-uri-string* t)

;; RFC 2396 standard URI components
(defvar %uri-scheme)
(defvar %uri-authority)
(defvar %uri-path)
(defvar %uri-query)
(defvar %uri-fragment)

;; extended URI components
(defvar %uri-head)
(defvar %uri-tail)
(defvar %uri-user)
(defvar %uri-host)
(defvar %uri-port)
(defvar %uri-directory)
(defvar %uri-file)

(defmacro bind-authority-components (authority &body body)
  (let ((t1 (gensym)) (t2 (gensym)))
    `(destructuring-bind (&optional ,t1 %uri-user %uri-host ,t2 %uri-port)
         (coerce
          (nth-value
           1
           (cl-ppcre:scan-to-strings "(([^@]+)@)?([^\\:]+)(\\:(\\d+))?"
                                     ,authority))
          'list)
       (declare (ignore ,t1 ,t2))
       ,@body)))

(defmacro bind-path-components (path &body body)
  `(destructuring-bind (&optional %uri-directory %uri-file)
       (coerce
        (nth-value
         1
         (cl-ppcre:scan-to-strings "(.*/)([^/]+)?"
                                   ,path))
        'list)
     ,@body))

(defmacro bind-standard-uri-components (uri-reference &body body)
  (let ((t1 (gensym)) (t2 (gensym)) (t3 (gensym)) (t4 (gensym)))
    `(destructuring-bind (&optional %uri-head ,t1 %uri-scheme ,t2 %uri-authority %uri-tail %uri-path ,t3 %uri-query ,t4 %uri-fragment)
         (coerce
          (nth-value
           1
           (cl-ppcre:scan-to-strings
            ;; regex adapted from RFC 2396
            ;; "^(([^:/?#]+):)?(//([^/?#]*))?([^?#]*)(\\?([^#]*))?(#(.*))?"
            "^((([^:/?#]+):)?(//([^/?#]*)))?(([^?#]*)(\\?([^#]*))?(#(.*))?)"
            ,uri-reference))
          'list)
       (declare (ignore ,t1 ,t2 ,t3 ,t4))
       (bind-authority-components %uri-authority
         (bind-path-components %uri-path
           ,@body)))))

(defmacro uri-template-bind ((template) uri &body body)
  "Binds URI template placeholders (which must be either symbols or
lists like (#'parse-fn var), much like arguments to
cl-ppcre:register-groups-bind) in given URI to the specified
variables, as well as binding a set of standard special
variables (%uri-protocol, %uri-host, etc.) to their respective parts
of the given URI."
  (let* ((template (cdr template)) ;; template is expected to look like output of #U: '(uri-template &rest args)
         (template-bindings (mapcar #'second (remove-if #'stringp template)))
         (template-vars (loop for x in template-bindings when (symbolp x) collect x))
         (uri-var (gensym)))
    `(let ((,uri-var ,uri))
       (bind-standard-uri-components ,uri-var
         (register-groups-bind ,template-bindings
             ('(:sequence
                :start-anchor
                ,@(substitute-if-not '(:REGISTER (:GREEDY-REPETITION 0 NIL :EVERYTHING))
                                     #'stringp template)
                :end-anchor)
               ,uri-var)
           ,(when *decode-uri-string*
                  `(setf ,@(mapcan (lambda (var)
                                     `(,var (kmrcl:decode-uri-string ,var)))
                                   template-vars)))
           ,@body)))))
