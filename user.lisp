(in-package :user)

(defstruct user
  name
  display-name
  date-created
  email
  date-of-birth
  gender
  interests
  country
  city)
  
(defun get-user-info-field-value (key-elem)
  (stp:string-value (stp:next-sibling key-elem)))

(defun table-keyp (elem  cell-name)
  (and (typep elem 'stp:element)
       (equal (stp:string-value elem) cell-name)
       (equal (stp:local-name elem) "td")))

(defun html-to-xml (html)
  (chtml:parse html (cxml-stp:make-builder)))

(defmacro extract-user-info (element-name struct-name name-field-pairs-list)
  "Helper macro that takes list of cons cells, which car is table entry name ~
and cdr is structure field accessor function, and returns condition form to be ~
used in recursive parsing process"
  (let ((cond-list '((t 'nil))))
    (dolist (current-pair name-field-pairs-list)
      (let ((key (car current-pair))
	    (field-accessor (cdr current-pair)))
	(setf cond-list (cons `((table-keyp ,element-name ,key)
				(handler-case 
				    (setf (,field-accessor ,struct-name)
					  (get-user-info-field-value ,element-name))
				  (stp:stp-error () nil)))
			      cond-list))))
    `(cond ,@cond-list)))

(defun parse-interests (interests)
  "Helper function for parsing interest list"
  (labels ((real-parse-interests (int-str int-list)
	     (if (position #\, int-str)
		 (real-parse-interests
		  (subseq int-str (+ 2 (position #\, int-str)))
		  (cons (subseq int-str 0 (position #\, int-str))
			int-list))
		 (cons int-str int-list))))
    (real-parse-interests interests nil)))
			
(defun fill-user-info (username base-url)
  "Creates fresh user structure, fetches and parses information and fills structure's fields"
  (let ((xml (html-to-xml (delete-every 
			   '(#\So #\Bel #\Dc4 #\Nak #\Vt) 
			   (drakma:http-request
			    (make-user-info-url username base-url)
			    :user-agent :firefox))))
	(current-user (make-user)))
    (stp:do-recursively (elem xml)
      (extract-user-info elem current-user
			 (("Имя" . user-display-name)
			  ("Дата создания" . user-date-created)
			  ("Адрес электронной почты  " . user-email)
			  ("Дата рождения" . user-date-of-birth)
			  ("Пол" . user-gender)
			  ("Интересы" . user-interests)
			  ("Страна" . user-country)
			  ("Город" . user-city))))
    (setf (user-name current-user) username)
    (setf (user-email current-user)
	  (remove #\SOFT_HYPHEN (user-email current-user)))
    (setf (user-interests current-user)
	  (parse-interests (user-interests current-user)))
    current-user))

(defun get-online-users (base-url)
  "Gets cons cell, car of wich is list of currently online users, and cdr is of recently went offline ones"
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
			 (if (string= (subseq homepage 7 15)
				      (concatenate 'string base-url "/"))
			     (cons (subseq homepage 21 (1- (length homepage)))
				   usernames)
			     (cons (subseq homepage 7 (position #\. homepage))
				   usernames))))))
	     usernames)))
    (let ((xml (html-to-xml (drakma:http-request
			     (concatenate 'string "http://" base-url "/online/")
			     :user-agent :firefox)))
	  (online nil)
	  (offline nil))
      (stp:do-recursively (elem xml)
	;;; Todo: process case when there is not enough users, so 
	;;; link "Показать полный список" does not appear
	(when (and (typep elem 'stp:element)
		   (equal (stp:local-name elem) "a")
		   (equal (stp:string-value elem) "Показать полный список"))
	  (if (null online)
	      (setf online (extract-userlist elem))
	      (setf offline (extract-userlist elem)))))
      (cons online offline))))

(defun store-user-in-database (user db datetime)
  (flet ((replace-quote (string)
	   (tools:replace-all string #\' "''")))
    (sqlite:execute-single 
     db
     (concatenate 'string 
		  "INSERT OR REPLACE INTO user(name, display_name, date_created, "
		  "date_of_birth, gender, country, city, last_active) "
		  "VALUES('"
		  (replace-quote (user-name user)) "', '"
		  (replace-quote (user-display-name user)) "', datetime('"
		  (user-date-created user) "'), "
		  (if (user-date-of-birth user)
		      (concatenate 'string
				   "datetime('" (user-date-of-birth user) "'),")
		      "NULL,")
		  (or (and (string= (user-gender user) "Женский") "1")
		      (and (string= (user-gender user) "Мужской") "2")
		      "0") ", '"
		      (replace-quote (user-country user)) "', '"
		      (replace-quote (user-city user)) "', "
		      "datetime('" datetime "'));"))
    (dolist (interest (user-interests user))
      (sqlite:execute-single
       db
       (concatenate 'string
		    "INSERT OR IGNORE INTO interest(name) "
		    "VALUES('" (replace-quote interest) "');"))
      (sqlite:execute-single
       db
       (concatenate 'string 
		    "INSERT INTO activity(user_name, time) "
		    "VALUES('" (replace-quote (user-name user)) "', "
		    "datetime('now', 'localtime'));"))
      (sqlite:execute-single
       db
       (concatenate 'string 
		    "INSERT OR REPLACE INTO interests_users_map("
		    "user_name, interest_name) VALUES('"
		    (replace-quote (user-name user)) "', '"
		    (replace-quote interest) "');")))))

(defun capture (base-url)
  "Main entry point. Automated script that registers recent actvity and stores information about users"
  (database:initialize-database)
  (log:write-log :info "Initialized database")
  (sqlite:with-open-database (db tools:*database-path*)
    (log:write-log :info "Attached to the database")
    (let* ((users (get-online-users base-url))
	   (online (car users))
	   (offline (cdr users))
	   (total (+ (length online)
		     (length offline)))
	   (progress 1)
	   (datetime-now (tools:format-date)))
      (log:write-log :info (format nil "Got users: ~A online, ~A just went offline"
				   (length online)
				   (length offline)))
      (dolist (current-user (concatenate 'list online offline))
	(log:write-log :info (format nil
				       "Processing user ~A ~A/~A" 
				       current-user
				       progress
				       total))
	(store-user-in-database (fill-user-info current-user base-url) 
				db datetime-now)
	(incf progress))))
  (log:write-log :info "Finished processing"))

(defun store-online-offline-in-database (online offline db)
  (sqlite:execute-single 
   db
   (concatenate 'string 
		"INSERT INTO site_online_magnitude(datetime, online, offline)"
		" VALUES(datetime('now','localtime'), "
		(write-to-string online) ", "
		(write-to-string offline) ");")))

(defun get-and-store-online (base-url)
  (let* ((users (get-online-users base-url))
	 (online (length (car users)))
	 (offline (length (cdr users))))
    (log:write-log :info (format nil "Got ~A online and ~A offline users"
				 online offline))
    (sqlite:with-open-database (db tools:*database-path*)
      (store-online-offline-in-database online offline db))
    (log:write-log :info "Successfully stored activity information in the database")))
