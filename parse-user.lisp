(in-package :parse-user)

(defstruct user
  name
  date-created
  gender
  data-of-birth
  country
  city
  interests)
  
(defun get-value (elem)
  (stp:string-value (stp:next-sibling elem)))

(defmacro when-table-key ((elem  cell-name) &body body)
  `(when (and (typep ,elem 'stp:element)
	     (equal (stp:string-value ,elem) ,cell-name)
	     (equal (stp:local-name ,elem) "td"))
     (progn ,@body)))

(defun get-user-page (username)
  (let ((html (chtml:parse (first (drakma:http-request
				   (make-user-info-url username)
				   :user-agent :firefox))
			   (cxml-stp:make-builder)))
	(user (make-user)))
    (stp:do-recursively (elem xml)
      (when (and (typep elem 'stp:element)
		 (equal (stp:strin
		       
    
