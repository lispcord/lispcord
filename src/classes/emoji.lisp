(in-package :lispcord.classes)

(defclass partial-emoji ()
  ((name :initarg :name :accessor name)
   (image :initarg :image :accessor image)
   (roles :initarg :roles :accessor roles)))

(defun make-emoji (name image &optional roles)
  (make-instance 'new-emoji
                 :name name
                 :image image
                 :roles roles))

(defmethod %to-json ((e partial-emoji))
  (with-object
    (write-key-value "name" (name e))
    (write-key-value "image" (image e))
    (write-key-value "roles" (or (roles e) :null))))

(defclass emoji ()
  ((id       :initarg :id
             :type snowflake
             :accessor id)
   (name     :initarg :name
             :type string
             :accessor name)
   (roles    :initarg :roles
             :type (vector snowflake)
             :accessor roles)
   (user     :initarg :user
             :type (or null user)
             :accessor user)
   (colons?  :initarg :colons?
             :type t
             :accessor colonsp)
   (managed  :initarg :managed
             :type t
             :accessor managedp)
   (animated :initarg :animated
             :type boolean
             :accessor animatedp)
   (guild-id :initarg :gid
             :type (or null snowflake)
             :accessor guild-id)))

(defmethod guild ((e emoji))
  (getcache-id (guild-id e) :guild))


(defmethod from-json ((c (eql :emoji)) (table hash-table))
  (instance-from-table (table 'emoji)
                       :id (parse-snowflake (gethash "id" table))
                       :name "name"
                       :roles (mapvec #'parse-snowflake (gethash "roles" table))
                       :user (cache :user (gethash "user" table))
                       :colons? "require_colons"
                       :managed "managed"
                       :animated "animated"))

(defmethod update ((table hash-table) (e emoji))
  (from-table-update (table data)
                     ("id" (id e) (parse-snowflake data))
                     ("name" (name e) data)
                     ("roles" (roles e) (mapvec #'parse-snowflake data))
                     ("require_colons" (colonsp e) data)
                     ("managed" (managedp e) data)
                     ("animated" (animatedp e) data))
  e)

(defmethod %to-json ((e emoji))
  (with-object
    (write-key-value "id" (id e))
    (write-key-value "name" (name e))
    (write-key-value "roles" (roles e))
    (write-key-value "require_colons" (colonsp e))
    (write-key-value "managed" (managedp e))
    (write-key-value "animated" (animatedp e))))


