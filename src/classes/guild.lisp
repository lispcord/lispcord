(in-package :lispcord.classes.guild)


;; this is a place holder until the actual roles are implemented!
(deftype role () 'snowflake)

(defclass member ()
  ;; I'm not really sure, but it should be possible to link this to a
  ;; specific object right?
  ((user      :initarg :user
	      :type user)
   (nick      :initarg :nick
	      :type (or null string))
   (roles     :initarg :roles
	      :type (vector role))
   (joined-at :initarg :joined-at
	      :type string)
   (deaf      :initarg :deaf
	      :type t)
   (mute      :initarg :mute
	      :type t)))

;;The Modify and Add Member REST-calls can use this
(defmethod %to-json ((m member))
  (with-object
    (write-key-val "user" (!! m user))
    (write-key-val "nick" (!! m nick))
    (write-key-val "roles" (!! m roles))
    (write-key-val "joined_at" (!! m joined-at))
    (write-key-val "mute" (!! m mute))
    (write-key-val "deaf" (!! m deaf))))

(defmethod from-json ((c (eql :member)) (table hash-table))
  (instance-from-table (table 'member)
    :user "user"
    :nick "nick"
    :roles "roles"
    :joined-at "joined_at"
    :mute "mute"
    :deaf "deaf"))
