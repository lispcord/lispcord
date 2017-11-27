(in-package :lispcord.classes)

(defclass overwrite ()
  ((id    :initarg :id
	  :type snowflake
	  :accessor id)
   (type  :initarg :type
	  :type string
	  :accessor type)
   (allow :initarg :allow
	  :type fixnum
	  :accessor allow)
   (deny  :initarg :deny
	  :type fixnum
	  :accessor deny)))

(defmethod from-json ((c (eql :overwrite)) (table hash-table))
  (instance-from-table (table 'overwrite)
    :id (parse-snowflake (gethash "id" table))
    :type "type"
    :allow "allow"
    :deny "deny"))

(defmethod %to-json ((o overwrite))
  (with-object
    (write-key-value "id" (id o))
    (write-key-value "type" (type o))
    (write-key-value "allow" (allow o))
    (write-key-value "deny" (deny o))))

(defclass channel ()
  ((id :initarg :id
       :type snowflake
       :accessor id)))

(defclass guild-channel (channel)
  ((guild-id      :initarg :g-id
		  :type (or null snowflake)
		  :accessor guild-id)
   (name          :initarg :name
		  :type string
		  :accessor name)
   (position      :initarg :pos
		  :type fixnum
		  :accessor position)
   (overwrites    :initarg :overwrites
		  :type (vector overwrite)
		  :accessor overwrites)
   (parent-id     :initarg :parent-id
		  :type (or null snowflake)
		  :accessor parent-id)))

(defclass category (guild-channel)
  ((nsfw :initarg :nsfw
	 :type t
	 :accessor nsfw-p)))

(defclass text-channel (guild-channel)
  ((nsfw         :initarg :nsfw
		 :type t
		 :accessor nsfw-p)
   (topic        :initarg :topic
		 :type (or null string)
		 :accessor topic)
   (last-message :initarg :last-message
		 :type (or null snowflake)
		 :accessor last-message)
   (last-pinned  :initarg :last-pinned
		 :type (or null string)
		 :accessor last-pinned)))

(defclass voice-channel (guild-channel)
  ((bitrate    :initarg :bitrate
	       :type fixnum
	       :accessor bitrate)
   (user-limit :initarg :user-limit
	       :type fixnum
	       :accessor user-limit)))

(defclass dm-channel (channel)
  ((last-message :initarg :last-message
		 :type (or null snowflake)
		 :accessor last-message)
   (recipients   :initarg :last-message
		 :type (vector user)
		 :accessor recipients)
   (last-pinned  :initarg :last-pinned
		 :type string
		 :accessor last-pinned)))

(defclass group-dm (dm-channel)
  ((name     :initarg :name
	     :type string
	     :accessor name)
   (icon     :initarg :icon
	     :type (or null string)
	     :accessor icon)
   (owner    :initarg :owner
	     :type snowflake
	     :accessor owner)))

(defun %guild-text-fj (table)
  (instance-from-table (table 'text-channel)
    :id (parse-snowflake (gethash "id" table))
    :g-id (parse-snowflake (gethash "guild_id" table))
    :name "name"
    :pos "position"
    :overwrites (mapvec (curry #'from-json :overwrite)
		     (gethash "permission_overwrites" table))
    :nsfw "nsfw"
    :topic "topic"
    :last-message (parse-snowflake (gethash "last_message_id" table))
    :parent-id (parse-snowflake (gethash "parent_id" table))
    :last-pinned "last_pin_timestamp"))

(defun %guild-voice-fj (table)
  (instance-from-table (table 'voice-channel)
    :id (parse-snowflake (gethash "id" table))
    :g-id (parse-snowflake (gethash "guild_id" table))
    :name "name"
    :pos "position"
    :overwrites (mapvec (curry #'from-json :overwrite)
		     (gethash "permission_overwrites" table))
    :bitrate "bitrate"
    :user-limit "user_limit"
    :parent-id (parse-snowflake (gethash "parent_id" table))))

(defun %dm-fj (table)
  (instance-from-table (table 'dm-channel)
    :last-message (parse-snowflake (gethash "last_message_id" table))
    :id (parse-snowflake (gethash "id" table))
    :recipients (mapvec (curry #'cache :user)
			(gethash "recipients" table))
    :last-pinned "last_pin_timestamp"))

(defun %group-dm-fj (table)
  (instance-from-table (table 'group-dm)
    :id (parse-snowflake (gethash "id" table))
    :name "name"
    :recipients (mapvec (curry #'cache :user)
		     (gethash "recipients" table))
    :last-message (parse-snowflake (gethash "last_message_id" table))
    :owner (parse-snowflake (gethash "owner_id" table))
    :last-pinned "last_pin_timestamp"))

(defun %guild-category-fj (table)
  (instance-from-table (table 'category)
    :id (parse-snowflake (gethash "id" table))
    :overwrites (mapvec (curry #'from-json :overwrite)
		     (gethash "permission_overwrites" table))
    :name "name"
    :parent-id (parse-snowflake (gethash "parent_id" table))
    :nsfw "nsfw"
    :pos "position"
    :g-id (parse-snowflake (gethash "guild_id" table))))

(defmethod from-json ((c (eql :channel)) (table hash-table))
  (case (gethash "type" table)
    (0 (%guild-text-fj table))
    (1 (%dm-fj table))
    (2 (%guild-voice-fj table))
    (3 (%group-dm-fj table))
    (4 (%guild-category-fj table))
    (otherwise (dprint :error "Channel type not recognised!~%This should only happen if discord creates a new channel type and lispcord wasn't updated yet~%Please file an issue at https://github.com/MegaLoler/lispcord/issues"))))

(defmethod update ((table hash-table) (c channel))
  (from-table-update (table data)
    ("id" (id c) (parse-snowflake data))
    ("guild_id" (guild-id c) (parse-snowflake data))
    ("position" (position c) data)
    ("permission_overwrites"
     (overwrites c) (mapvec (curry #'from-json :overwrite)
			    data))
    ("name" (name c) data)
    ("topic" (topic c) data)
    ("nsfw" (nsfw-p c) data)
    ("last_message_id"
     (last-message c) (parse-snowflake data))
    ("bitrate" (bitrate c) data)
    ("user_limit" (user-limit c) data)
    ("recipients"
     (recipients c) (mapvec (curry #'cache :user)
			    data))
    ("icon" (icon c) data)
    ("owner_id" (owner c) (parse-snowflake data))
    ("application_id" (app-id c) (parse-snowflake data))
    ("parent_id" (parent-id c) (parse-snowflake data))
    ("last_pin_timestamp" (last-pinned c) data))
  c)

(defmethod %to-json ((gtc text-channel))
  (with-object
    (write-key-value "id" (id gtc))
    (write-key-value "guild_id" (guild-id gtc))
    (write-key-value "name" (name gtc))
    (write-key-value "permission_overwrites" (overwrites gtc))
    (write-key-value "parent_id" (parent-id gtc))
    (write-key-value "nsfw" (nsfw-p gtc))
    (write-key-value "topic" (topic gtc))
    (write-key-value "last_message_id" (last-message gtc))
    (write-key-value "last_pin_timestamp" (last-pinned gtc))))

(defmethod %to-json ((gvc voice-channel))
  (with-object
    (write-key-value "id" (id gvc))
    (write-key-value "guild_id" (guild-id gvc))
    (write-key-value "name" (name gvc))
    (write-key-value "permission_overwrites" (overwrites gvc))
    (write-key-value "parent_id" (parent-id gvc))
    (write-key-value "bitrate" (bitrate gvc))
    (write-key-value "user_limit" (user-limit gvc))))

(defmethod %to-json ((dm dm-channel))
  (with-object
    (write-key-value "id" (id dm))
    (write-key-value "last_message_id" (last-message dm))
    (write-key-value "recipients" (recipients dm))))

(defmethod %to-json ((gdm group-dm))
  (with-object
    (write-key-value "id" (id gdm))
    (write-key-value "name" (name gdm))
    (write-key-value "icon" (icon gdm))
    (write-key-value "owner_id" (owner gdm))))


