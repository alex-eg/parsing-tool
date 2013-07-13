(in-package :database)

(defun initialize-database ()
  "Initialize database, create and/or validate schema"
  (write-log :info "Initializing database...")
  (sqlite:with-open-database  (db *database*)
    (sqlite:execute-single 
     db
     "CREATE TABLE IF NOT EXISTS user(
id INTEGER UNIQUE NOT NULL PRIMARY KEY,
name VARCHAR(255),
date_created DATETIME,
email VARCHAR(255),
date_of_birth VARCHAR(255),
gender TINYINT,
country VARCHAR(255),
city VARCHAR(255),
last_active DATETIME);")
    (sqlite:execute-single
     db
     "CREATE TABLE IF NOT EXISTS interest(
id INTEGER UNIQUE NOT NULL PRIMARY KEY,
name VARCHAR(255),
real_id INTEGER);") ;; Real id - for things like l33t and leet and different writings of same word
    (sqlite:execute-single
     db
     "CREATE TABLE IF NOT EXISTS interests_users_map(
user_id INTEGER,
interest_id INTEGER,
FOREIGN KEY (user_id) REFERENCES user(id) ON UPDATE CASCADE ON DELETE CASCADE,
FOREIGN KEY (interest_id) REFERENCES interest(id));")))


