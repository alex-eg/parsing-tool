#!/usr/bin/sbcl --script 

(load #P"/etc/sbclrc")
(load (merge-pathnames (user-homedir-pathname) #P"/.sbclrc"))

(load #P"parser.asd")
(asdf:operate 'asdf:load-op 'parsing-tools)
(user:capture (cadr sb-ext:*posix-argv*))
