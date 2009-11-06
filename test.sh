#! /bin/sh

rm *.fasl
mv test.output test.output~
echo "
(require 'asdf)
(require 'cl-uri-templates.test)" | \
    sbcl && \
    less test.output
