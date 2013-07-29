(in-package :tools)

(defparameter *database-path*  (merge-pathnames *default-pathname-defaults* #P"./database.sqlite"))

(defun make-diary-url (username base-url)
  (concatenate 'string "http://" username "." base-url "/"))

(defun make-user-info-url (username base-url)
  (concatenate 'string "http://" base-url "/users/" username "/"))

(defmacro referer-query (referer useragent usernames)
  (let ((uname (gensym)))
    `(dolist (,uname ,usernames)
       (drakma:http-request (make-diary-url ,uname) :user-agent ,useragent
			    :additional-headers '(("Referer" . ,referer))))))

(defun replace-all (string char replacement-string)
  (let ((list (coerce string 'list)))
    (with-output-to-string (out)
      (dolist (c list)
	(if (char-equal c char)
	    (write-string replacement-string out)
	    (write-char c out))))))
