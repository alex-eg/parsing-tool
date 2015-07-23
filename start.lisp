(in-package :parsing-tool)

(defparameter +option-spec+
  '((("base-url" #\s) :type string)
    (("store-online") :type boolean :optional t)))

(defun main (args)
  (handle-command-line
   +option-spec+
   'process
   :command-line args
   :name "parsing-tool"
   :rest-arity nil))

(defun process (&key base-url store-online)
  (let ((username (cdr (assoc 'username *db-credentials*)))
        (db-name  (cdr (assoc 'db-name  *db-credentials*)))
        (password (cdr (assoc 'password *db-credentials*)))
        (host     (cdr (assoc 'host     *db-credentials*))))
    (unwind-protect
         (progn
           (initialize-database db-name username password host)
           (write-log :info "Initialized database")
           (if store-online
               (get-and-store-online base-url)
               (capture base-url)))
      (disconnect-toplevel))))
