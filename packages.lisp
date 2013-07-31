(defpackage :tools
  (:use :cl)
  (:export :make-diary-url
	   :make-user-info-url
	   :referer-query
	   :replace-all 
	   :delete-every
	   :+database-path+
	   :+thread-number+))  

(defpackage :log
  (:use :cl)
  (:export :write-log))

(defpackage :user
  (:use :cl :tools :log)
  (:export :capture
	   :get-online-users))
	   

(defpackage :database
  (:use :cl :tools :sqlite :log)
  (:export :initialize-database))

