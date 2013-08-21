(defpackage :tools
  (:use :cl)
  (:export :make-diary-url
	   :make-user-info-url
	   :referer-query
	   :replace-all 
	   :delete-every
	   :format-date
	   :sqlite-select
	   :sqlite-select-to-str
	   :*database-path*))  

(defpackage :log
  (:use :cl)
  (:export :write-log))

(defpackage :user
  (:use :cl :tools :log)
  (:export :capture
	   :get-and-store-online
	   :get-online-users))
	   

(defpackage :database
  (:use :cl :tools :sqlite :log)
  (:export :initialize-database))

