(in-package :log)

(defparameter *logfile* *standard-output*)
(defparameter *loglevel* :error)

(defmacro write-log (level message)
  (let* ((log-levels (list :debug :info :warning :error :critical))
	 (current-level (position *loglevel* log-levels)))
    (if (not (member level log-levels))
	(error "~A is not a valid loglevel" (symbol-name level)))
    (if (>= (position level log-levels)
	    current-level)
	`(format ,*logfile* "[~A] ~A" (symbol-name ,level) ,message))))
