(in-package :log)

(defparameter *logfile* '*standard-output*)
(defparameter *loglevel* :debug)

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
    

(defmacro write-log (level message)
  (let* ((log-levels (list :debug :info :warning :error :critical))
	 (current-level (position *loglevel* log-levels)))
    (if (not (member level log-levels))
	(error "~A is not a valid loglevel" (symbol-name level)))
    (if (>= (position level log-levels)
	    current-level)
	`(format ,*logfile* "[~A] [~A] ~A~%" 
		 (format-date)
		 (symbol-name ,level) ,message))))

	
