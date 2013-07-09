(defpackage :tools
  (:use :cl)
  (:export :make-diary-url
	   :make-user-info-url
	   :referer-query))  

(defpackage :user
  (:use :cl :tools)
  (:export :get-online-users
	   :fill-user-info))
