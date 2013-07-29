#!/usr/bin/sbcl --script 

(load #P"parser.asd")
(asdf:operate 'asdf:load-op 'parsing-tools)
(user:capture)
