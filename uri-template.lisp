(in-package "URI-TEMPLATE")

(defvar *encode-uri-string* t)

(defun read-uri-template (stream &optional recursive-p)
  (let ((*readtable* (copy-readtable))
        (token-accumulator ())
        (string-accumulator ()))
    (flet ((collect-string ()
             (when string-accumulator
               (push (coerce (reverse string-accumulator) 'string) token-accumulator)
               (setf string-accumulator ()))))
      (set-syntax-from-char #\} #\Space)
      (let (next-char)
        (loop until (member (setf next-char (read-char stream nil #\Space recursive-p)) '(#\Space #\Newline #\Tab #\))) do
              (case next-char
                (#\{ (collect-string)
                     (let ((sexp (read stream t nil recursive-p)))
                       (push `(maybe-uri-encode ,sexp) token-accumulator)))
                (#\})
                (t (push next-char string-accumulator)))
              finally (unread-char next-char stream) (collect-string)))
      (reverse token-accumulator))))

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
