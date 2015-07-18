(in-package :parsing-tool)

(defun initialize-database (db-name username password host)
  "Initialize database, create and/or validate schema"
  (write-log :info "Initializing database...")
  (connect-toplevel db-name username password host)
  (deftable user
    (!dao-def))
  (deftable interest
    (!dao-def))
  (deftable activity
    (!dao-def)
    (!foreign 'users 'user-name 'name))
  (deftable community
    (!dao-def))
  (deftable user-interest-map
    (!dao-def)
    (!foreign 'users 'user-name 'name)
    (!foreign 'interests 'interest-name 'name))
  (deftable site-online-log
    (!dao-def))

  (dolist (dao-class (list 'user 'interest 'activity 'community 'user-interest-map 'site-online-log))
    (unless (table-exists-p (dao-table-name dao-class))
      (create-table dao-class))))
