(in-package :tools)

(defun make-homepage-url (username)
  (concatenate 'string "http://" username ".beon.ru/"))

(defun make-user-info-url (username)
  (concatenate 'string "http://beon.ru/users/" username "/"))

(defmacro referer-query (referer useragent usernames)
  (let ((uname (gensym)))
    `(dolist (,uname ,usernames)
       (drakma:http-request (make-homepage-url ,uname) :user-agent ,useragent
			    :additional-headers '(("Referer" . ,referer))))))
