(in-package :lispcord.classes)


(defclass emoji ()
  ((id      :initarg :id
	    :type snowflake
	    :accessor id)
   (name    :initarg :name
	    :type string
	    :accessor name)
   (roles   :initarg :roles
	    :type (vector snowflake)
	    :accessor roles)
   (user    :initarg :user
	    :type (or null user)
	    :accessor user)
   (colons? :initarg :colons?
	    :type t
	    :accessor colonsp)
   (managed :initarg :managed
	    :type t
	    :accessor managedp)))


(defmethod from-json ((c (eql :emoji)) (table hash-table))
  (instance-from-table (table 'emoji)
    :id (parse-snowflake (gethash "id" table))
    :name "name"
    :roles (map 'vector #'parse-snowflake (gethash "roles" table))
    :user (cache :user (gethash "user" table))
    :colons? "require_colons"
    :managed "managed"))

(defmethod update ((table hash-table) (e emoji))
  (from-table-update (table data)
    ("id" (id e) (parse-snowflake data))
    ("name" (name e) data)
    ("roles" (roles e) (mapvec #'parse-snowflake data))
    ("require_colons" (colonsp e) data)
    ("managed" (managedp e) data))
  e)

(defmethod %to-json ((e emoji))
  (with-object
    (write-key-value "id" (id e))
    (write-key-value "name" (name e))
    (write-key-value "roles" (roles e))
    (write-key-value "require_colons" (colonsp e))
    (write-key-value "managed" (managedp e))))


