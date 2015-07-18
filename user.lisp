(in-package :parsing-tool)

(defun get-user-info-field-value (key-elem)
  (stp:string-value (stp:next-sibling key-elem)))

(defun table-keyp (elem  cell-name)
  (and (typep elem 'stp:element)
       (equal (stp:string-value elem) cell-name)
       (equal (stp:local-name elem) "td")))

(defun html-to-xml (html)
  (chtml:parse html (cxml-stp:make-builder)))

(defmacro extract-user-info (element-name object-name name-field-pairs-list)
  "Helper macro that takes list of cons cells, which car is table entry name
and cdr is structure field accessor function, and returns condition form to be
used in recursive parsing process"
  (let ((cond-list '((t 'nil))))
    (dolist (current-pair name-field-pairs-list)
      (let ((key (car current-pair))
	    (slot-accessor (cdr current-pair)))
	(setf cond-list (cons `((table-keyp ,element-name ,key)
				(handler-case 
				    (setf (,slot-accessor ,object-name)
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
			   '(#\So #\Bel #\Dc4 #\Nak #\Vt #\Etx #\Nul #\Soh)
			   (drakma:http-request
			    (make-user-info-url username base-url)
			    :user-agent :firefox))))
        (user (make-instance 'user :name username)))
    (stp:do-recursively (elem xml)
      (extract-user-info elem user
			 (("Имя" . user-display-name)
			  ("Дата создания" . user-date-created)
			  ("Адрес электронной почты  " . user-email)
			  ("Дата рождения" . user-date-of-birth)
			  ("Пол" . user-gender)
			  ("Интересы" . user-interests)
			  ("Страна" . user-country)
			  ("Город" . user-city))))
    (setf (user-email user)
	  (remove #\SOFT_HYPHEN (user-email user)))
    (setf (user-interests user)
	  (parse-interests (user-interests user)))
    (unless (eql :null (user-date-of-birth user))
      (setf (user-date-of-birth user)
            (parse-date (user-date-of-birth user))))
    user))

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

(defun store-user-in-database (user)
  (flet ((replace-quote (string)
	   (replace-all string #\' "''")))
    (with-slots (last-active gender) user
      (setf last-active (universal-time-to-timestamp (get-universal-time)))
      (setf gender (or (and (string= (user-gender user) "Женский") 1)
                       (and (string= (user-gender user) "Мужской") 2)
                       0)))
    (upsert-dao user)
    (dolist (interest-name (user-interests user))
      (unless (null interest-name)
        (let ((interest-name-normalized (normalize-interest-name interest-name)))
          (when (null (select-dao 'interest (:= 'name interest-name-normalized)))
            (let ((interest (make-instance 'interest :name interest-name-normalized)))
              (insert-dao interest)))
          (when (null (select-dao 'user-interest-map (:and (:= 'user-name (user-name user))
                                                           (:= 'interest-name interest-name-normalized))))
            (let ((interest-user (make-instance 'user-interest-map
                                                :user-name (user-name user)
                                                :interest-name interest-name-normalized)))
              (insert-dao interest-user))))))
    (let ((activity (make-instance 'activity
                                   :user-name (user-name user)
                                   :time (universal-time-to-timestamp (get-universal-time)))))
      (upsert-dao activity))))

(defun capture (base-url db-name username password host)
  "Main entry point. Automated script that registers recent actvity and stores information about users"
  (unwind-protect
       (progn
         (initialize-database db-name username password host)
         (write-log :info "Initialized database")
         (let* ((users (get-online-users base-url))
                (online (car users))
                (offline (cdr users))
                (total (+ (length online)
                          (length offline)))
                (progress 1))
           (write-log :info (format nil "Got users: ~A online, ~A just went offline"
                                    (length online)
                                    (length offline)))
           (dolist (current-user (concatenate 'list online offline))
             (write-log :info (format nil
                                      "Processing user ~A ~A/~A"
                                      current-user
                                      progress
                                      total))
             (store-user-in-database (fill-user-info current-user base-url) )
             (incf progress)))
         (write-log :info "Finished processing"))
    (disconnect-toplevel)))

(defun store-online-offline-in-database (online offline db)
  nil)

(defun get-and-store-online (base-url)
  (write-log :info "Successfully stored activity information in the database"))
