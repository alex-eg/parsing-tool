(in-package :database)

(defun initialize-database ()
  "Initialize database, create and/or validate schema"
  (write-log :info "Initializing database...")
  (sqlite:with-open-database  (db +database-path+)
    (sqlite:execute-single 
     db
     "CREATE TABLE IF NOT EXISTS user(
name VARCHAR(512) UNIQUE NOT NULL PRIMARY KEY,
display_name VARCHAR(512), 
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
name VARCHAR(1024) UNIQUE NOT NULL PRIMARY KEY,
real_name VARCHAR(1024));") ;; Real id - for things like l33t and leet and different spellings of same word
    (sqlite:execute-single
     db
     "CREATE TABLE IF NOT EXISTS activity(
user_name VARCHAR(512),
time DATETIME,
FOREIGN KEY (user_name) REFERENCES user(name));")
    (sqlite:execute-single
     db
     "CREATE TABLE IF NOT EXISTS interests_users_map(
user_name VARCHAR(512),
interest_name VARCHAR(1024),
FOREIGN KEY (user_name) REFERENCES user(name) ON UPDATE CASCADE ON DELETE CASCADE,
FOREIGN KEY (interest_name) REFERENCES interest(name));")))
  
		      
