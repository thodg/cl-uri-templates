(in-package "URI-TEMPLATE.TEST")

#.(enable-uri-template-syntax)

(defun run-tests ()
  (run-interpolation-tests)
  (run-destructuring-tests))
  
(defun run-interpolation-tests ()
  (let ((baz 1)
        (bar "bar"))
    (assert (string= #Uhttp://www.foo.com/bar/{baz}
                     "http://www.foo.com/bar/1"))
    (assert (string= #Uhttp://www.foo.com/bar/{bar}/{baz}
                     "http://www.foo.com/bar/bar/1"))
    (assert (string= #Uhttp://www.foo.com/bar/{bar}{baz}
                     "http://www.foo.com/bar/bar1"))
    (assert (string= #Uhttp://www.foo.com/bar?foo={"^BAZ"}
                     "http://www.foo.com/bar?foo=%5EBAZ"))))

(defun run-destructuring-tests ()
  (assert (equal (uri-template-bind (#Uhttp://www.factory.com/orders/{part}/{(#'parse-integer number)})
                     "http://www.factory.com/orders/widget/1234"
                   (list part number %uri-host))
                 '("widget" 1234 "www.factory.com")))
  (assert (equal (bind-standard-uri-components
                     "https://www.google.com/dir/1/2/search.html?arg=0-a&arg1=1-b&amp;arg3-c#hash"
                   (list %uri-scheme %uri-host %uri-path %uri-directory %uri-file %uri-query %uri-fragment))
                 '("https" "www.google.com" "/dir/1/2/search.html" "/dir/1/2/" "search.html" "arg=0-a&arg1=1-b&amp;arg3-c" "hash")))
  (assert (equal (uri-template:uri-template-bind (#U/apps/{app-name}/{table-template}/{table-id})
                     "/apps/EVWeb/Fixed%20Cost/20"
                   (list app-name table-template table-id))
                 '("EVWeb" "Fixed Cost" "20")))
  (assert (equal (bind-standard-uri-components
                     "http://files3.dsv.data.cod.ru/?WyIyNWQ3NWU5NTRmZDU1MWIzYmQ5NzVjNzJhZjRkZmNhZSIsMTI1MTA5NjMxNCwiXHUwNDEwXHUwNDNiXHUwNDRjXHUwNDRmXHUwNDNkXHUwNDQxIFx1MDQ0MVx1MDQzNVx1MDQ0MFx1MDQzZVx1MDQzYVx1MDQ0MFx1MDQ0Ylx1MDQzYlx1MDQ0Ylx1MDQ0NS5yYXIiLCJrTzNqSUo3bUN5WlBPenlBVGdcL0M3UkZVWHdXYkN6SWtEYzUweTl5a1lOVCtTRmlwVFdsN1UxWlVybGVLNjMyaGlYc0hvVDhGZitGWUt6eGVVRGxOVkxUN3R0MndLYjg4VGFjYmZSVnhrZjNYQXdZalpYemVEQXM4bmxzK0RCbnZEcnZQTmRMKytDS05pNjVJXC8yb2JnY0N1RmdyK1lpS0VSak8rNVZSeTIrcz0iXQ%3D%3D"
                   (list %uri-scheme %uri-authority %uri-path %uri-directory %uri-file %uri-query %uri-fragment))
                 '("http" "files3.dsv.data.cod.ru" "/" "/" nil "WyIyNWQ3NWU5NTRmZDU1MWIzYmQ5NzVjNzJhZjRkZmNhZSIsMTI1MTA5NjMxNCwiXHUwNDEwXHUwNDNiXHUwNDRjXHUwNDRmXHUwNDNkXHUwNDQxIFx1MDQ0MVx1MDQzNVx1MDQ0MFx1MDQzZVx1MDQzYVx1MDQ0MFx1MDQ0Ylx1MDQzYlx1MDQ0Ylx1MDQ0NS5yYXIiLCJrTzNqSUo3bUN5WlBPenlBVGdcL0M3UkZVWHdXYkN6SWtEYzUweTl5a1lOVCtTRmlwVFdsN1UxWlVybGVLNjMyaGlYc0hvVDhGZitGWUt6eGVVRGxOVkxUN3R0MndLYjg4VGFjYmZSVnhrZjNYQXdZalpYemVEQXM4bmxzK0RCbnZEcnZQTmRMKytDS05pNjVJXC8yb2JnY0N1RmdyK1lpS0VSak8rNVZSeTIrcz0iXQ%3D%3D" nil)))
  (assert (equal (bind-standard-uri-components
                     "http://www.foo.com/abc?bar=baz&xyz=1"
                   (list %uri-scheme %uri-host %uri-path %uri-directory %uri-file %uri-query %uri-fragment %uri-head %uri-tail))
                 '("http" "www.foo.com" "/abc" "/" "abc" "bar=baz&xyz=1" nil "http://www.foo.com" "/abc?bar=baz&xyz=1")))
  (assert (equal (bind-standard-uri-components
                     "http://www.foo.com/?bar=baz&xyz=1"
                   (list %uri-scheme %uri-host %uri-path %uri-directory %uri-file %uri-query %uri-fragment))
                 '("http" "www.foo.com" "/" "/" nil "bar=baz&xyz=1" nil)))
  (assert (equal (bind-standard-uri-components "http://user@host.com:8080/dir1/dir2/file?query=want&a=b#hash"
                   (list %uri-scheme %uri-authority (list %uri-user %uri-host %uri-port)
                         %uri-path (list %uri-directory %uri-file) %uri-query %uri-fragment))
                 '("http" "user@host.com:8080" ("user" "host.com" "8080") "/dir1/dir2/file"
                   ("/dir1/dir2/" "file") "query=want&a=b" "hash")))
  (assert (equal (bind-standard-uri-components "/foo/bar"
                   (list %uri-head %uri-tail %uri-scheme
                         %uri-authority (list %uri-user %uri-host %uri-port)
                         %uri-path (list %uri-directory %uri-file) %uri-query %uri-fragment))
                 '(NIL "/foo/bar" NIL NIL (NIL NIL NIL) "/foo/bar" ("/foo/" "bar") NIL NIL))))

