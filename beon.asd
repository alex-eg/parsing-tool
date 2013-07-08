(in-package :cl-user)

(defpackage :beon-tools-asd
  (:use :cl :asdf))

(in-package :beon-tools-asd)

(defsystem :beon-tools
  :components ((:file "packages")
	       (:file "tools")
	       (:file "parse-user"))
  :depends-on (:closure-html
	       :drakma
	       :cl-sqlite
	       :cxml-stp))
		 
