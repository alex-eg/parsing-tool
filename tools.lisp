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

(defun delete-every (char-list string)
  (let ((clean-string ""))
  (dolist (c char-list)
    (setf clean-string (delete c string)))
  clean-string))

(defun format-date ()
  (macrolet ((format-2-digits (name)
	       `(if (= (mod ,name 10) ,name)
		    (format nil "0~A" ,name)
		    (format nil "~A" ,name))))
    (multiple-value-bind (raw-seconds raw-minutes raw-hours
				      raw-day raw-month year)
	(get-decoded-time)
      (let ((seconds (format-2-digits raw-seconds))
	    (minutes (format-2-digits raw-minutes))
	    (hours (format-2-digits raw-hours))
	    (day (format-2-digits raw-day))
	    (month (format-2-digits raw-month)))
	(format nil "~A-~A-~A ~A:~A:~A"
		day
		month
		year
		hours
		minutes
		seconds)))))

(defun sqlite-select (select)
  (sqlite:with-open-database (db *database-path*)
    (sqlite:execute-to-list db select)))

(defun sqlite-select-to-str (request &optional (key #'car))
  "Performs sqlite request and iterates over response list, collecting #'keys of each element into a string"
    (with-output-to-string (str)
      (dolist (c (sqlite-select request))
	(prin1 (funcall key c))
	(write-char #\,))))
