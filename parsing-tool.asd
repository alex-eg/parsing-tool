(in-package :cl-user)

(defpackage :parsing-tool-asd
  (:use :cl :asdf))

(in-package :parsing-tool-asd)

(defsystem :parsing-tool
  :encoding :utf-8
  :components ((:file "packages")
	       (:file "tools")
	       (:file "log")
               (:file "model")
	       (:file "database")
	       (:file "user"))
  :depends-on (:closure-html
               :split-sequence
	       :drakma
               :simple-date
	       :postmodern
	       :cxml-stp))
