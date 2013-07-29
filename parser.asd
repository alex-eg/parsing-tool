(in-package :cl-user)

(defpackage :parsing-tools-asd
  (:use :cl :asdf))

(in-package :parsing-tools-asd)

(defsystem :parsing-tools
  :components ((:file "packages")
	       (:file "tools")
	       (:file "log")
	       (:file "database")
	       (:file "user"))
  :depends-on (:closure-html
	       :drakma
	       :sqlite
	       :cxml-stp))
		 
