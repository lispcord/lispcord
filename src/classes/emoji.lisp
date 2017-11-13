(in-package :lispcord.classes.emoji)

(defclass emoji ()
  ((id      :initarg :id
	    :type snowflake)
   (name    :initarg :name
	    :type string)
   (roles   :initarg :roles
	    :type (vector snowflake))
   (user    :initarg :user
	    :type user)
   (colons? :initarg :colons?
	    :type t)
   (managed :initarg :managed
	    :type t)))

(defmethod from-json ((c (eql :emoji)) (table hash-table))
  (instance-from-table (table 'emoji)
    :id "id"
    :name "name"
    :roles (map 'vector #'id (gethash "roles" table))
    :user "user"
    :colons? "require_colons"
    :managed "managed"))

(defmethod %to-json ((e emoji))
  (with-object
    (write-key-value "id" (!! e id))
    (write-key-value "name" (!! e name))
    (write-key-value "roles" (!! e roles))
    (write-key-value "require_colons" (!! e colons?))
    (write-key-value "managed" (!! e managed))))
