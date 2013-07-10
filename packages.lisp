(defpackage :tools
  (:use :cl)
  (:export :make-diary-url
	   :make-user-info-url
	   :referer-query
	   :*database*))  

(defpackage :user
  (:use :cl :tools)
  (:export :get-online-users
	   :fill-user-info))

(defpackage :database
  (:use :cl :tools :sqlite))

