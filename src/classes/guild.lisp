(in-package :lispcord.classes.guild)


(defclass role ()
  ((id          :initarg :id
		:type snowflake)
   (name        :initarg :name
		:type string)
   (color       :initarg :color
		:type fixnum)
   (hoist       :initarg :hoist
		:type t)
   (position    :initarg :pos
		:type fixnum)
   (permissions :initarg :perms
		:type fixnum)
   (managed     :initarg :managed
		:type t)
   (mentionable :initarg :mentionable
		:type t)))

(defmethod from-json ((c (eql :role)) (table hash-table))
  (instance-from-table (table 'role)
		       :id "id"
		       :name "name"
		       :color "color"
		       :hoist "hoist"
		       :pos "position"
		       :perms "permissions"
		       :managed "managed"
		       :mentionable "mentionable"))

(defmethod %to-json ((r role))
  (with-object
      (write-key-value "id" (!! r id))
    (write-key-value "name" (!! r name))
    (write-key-value "color" (!! r color))
    (write-key-value "hoist" (!! r hoist))
    (write-key-value "position" (!! r position))
    (write-key-value "permissions" (!! r permissions))
    (write-key-value "managed" (!! r managed))
    (write-key-value "mentionable" (!! r mentionable))))

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
    (write-key-value "user" (!! m user))
    (write-key-value "nick" (!! m nick))
    (write-key-value "roles" (!! m roles))
    (write-key-value "joined_at" (!! m joined-at))
    (write-key-value "mute" (!! m mute))
    (write-key-value "deaf" (!! m deaf))))

(defmethod from-json ((c (eql :member)) (table hash-table))
  (instance-from-table (table 'member)
    :user "user"
    :nick "nick"
    :roles "roles"
    :joined-at "joined_at"
    :mute "mute"
    :deaf "deaf"))
