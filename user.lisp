(in-package :user)

(defstruct user
  name
  date-created
  email
  date-of-birth
  gender
  interests
  country
  city
  last-active)
  
(defun get-user-info-field-value (key-elem)
  (stp:string-value (stp:next-sibling key-elem)))

(defun table-keyp (elem  cell-name)
  (and (typep elem 'stp:element)
       (equal (stp:string-value elem) cell-name)
       (equal (stp:local-name elem) "td")))

(defun html-to-xml (html)
  (chtml:parse html (cxml-stp:make-builder)))

(defmacro extract-user-info (element-name struct-name 
			     name-field-pairs-list)
  (let ((cond-list '((t 'nil))))
    (dolist (current-pair name-field-pairs-list)
      (let ((key (car current-pair))
	    (field-accessor (cdr current-pair)))
	(setf cond-list (cons `((table-keyp ,element-name ,key)
				(setf (,field-accessor ,struct-name)
				      (get-user-info-field-value ,element-name)))
			      cond-list))))
    `(cond ,@cond-list)))



(defun fill-user-info (username)
  (let ((xml (html-to-xml (drakma:http-request
			   (make-user-info-url username)
			   :user-agent :firefox)))
	(current-user (make-user)))
    (stp:do-recursively (elem xml)
      (extract-user-info elem current-user
			 (("Имя" . user-name)
			  ("Дата создания" . user-date-created)
			  ("Адрес электронной почты  " . user-email)
			  ("Дата рождения" . user-date-of-birth)
			  ("Пол" . user-gender)
			  ("Интересы" . user-interests)
			  ("Страна" . user-country)
			  ("Город" . user-city))))
    current-user))

(defun get-online-users ()
  (flet ((extract-userlist (elem)
	   (let* ((raw-script (stp:attribute-value elem "onclick"))
		  (len (length raw-script))
		  (online (subseq raw-script 38 (- len 16)))
		  (online-xml (html-to-xml online))
		  (usernames nil))
	     (stp:do-recursively (elem online-xml)
	       (when (and (typep elem 'stp:element)
			  (equal (stp:local-name elem) "a")
			  (equal (stp:attribute-value elem "class") "user"))
		 (let ((homepage (stp:attribute-value elem "href")))
		   (setf usernames
			 (if (string= (subseq homepage 7 15) "beon.ru/")
			     (cons (subseq homepage 21 (1- (length homepage)))
				   usernames)
			     (cons (subseq homepage 7 (position #\. homepage))
				   usernames))))))
	     usernames)))
    (let ((xml (html-to-xml (drakma:http-request
			     "http://beon.ru/online/"
			     :user-agent :firefox)))
	  (users nil))
      (stp:do-recursively (elem xml)
	(when (and (typep elem 'stp:element)
		   (equal (stp:local-name elem) "a")
		   (equal (stp:string-value elem) "Показать полный список"))
;;	  (format t "ONLINE ~S~%~%" (extract-userlist elem)))))))
	  (setf users (cons (extract-userlist elem) users))))
      users)))

(defun initialize-database ()
  "Initialize database, create and/or validate schema"
  (let ((db (sqlite:connect #P"./users.sqlite")))
    (sqlite:execute-non-query 
     db
     "CREATE TABLE IF NOT EXISTS users(name VARCHAR(255),
                                           date_created DATETIME,
                                           email VARCHAR(255),
                                           date_of_birth VARCHAR(255),
                                           gender TINYINT,
                                           country VARCHAR(255),
                                           city VARCHAR(255),
                                           last_active DATETIME);")))

                                                   


