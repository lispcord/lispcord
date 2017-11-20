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
	    :type user
	    :accessor user)
   (colons? :initarg :colons?
	    :type t
	    :accessor colonsp)
   (managed :initarg :managed
	    :type t
	    :accessor managedp)))


(defmethod from-json ((c (eql :emoji)) (table hash-table))
  (instance-from-table (table 'emoji)
    :id "id"
    :name "name"
    :roles (map 'vector #'identity (gethash "roles" table))
    :user "user"
    :colons? "require_colons"
    :managed "managed"))

(defmethod %to-json ((e emoji))
  (with-object
    (write-key-value "id" (id e))
    (write-key-value "name" (name e))
    (write-key-value "roles" (roles e))
    (write-key-value "require_colons" (colonsp e))
    (write-key-value "managed" (managedp e))))


