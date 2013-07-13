(defpackage :tools
  (:use :cl)
  (:export :make-diary-url
	   :make-user-info-url
	   :referer-query
	   :*database*))  

(defpackage :log
  (:use :cl)
  (:export :write-log))

(defpackage :user
  (:use :cl :tools :log)
  (:export :get-online-users
	   :fill-user-info))

(defpackage :database
  (:use :cl :tools :sqlite :log))

