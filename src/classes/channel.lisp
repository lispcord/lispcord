(in-package :lispcord.classes)

(defun overwrite-type-p (s)
  (and (stringp s) (or (string= s "member") (string= s "role"))))

(deftype overwrite-type ()
  `(and string
	(satisfies overwrite-type-p)))

(defclass overwrite ()
  ((id    :initarg :id
	  :type snowflake
	  :accessor id)
   (type  :initarg :type
	  :type overwrite-type
	  :accessor type)
   (allow :initarg :allow
	  :type fixnum
	  :accessor allow)
   (deny  :initarg :deny
	  :type fixnum
	  :accessor deny)))


(defmethod from-json ((c (eql :overwrite)) (table hash-table))
  (instance-from-table (table 'overwrite)
    :id "id"
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
		  :type snowflake
		  :accessor guild-id)
   (name          :initarg :name
		  :type string
		  :accessor name)
   (position      :initarg :pos
		  :type fixnum
		  :accessor position)
   (overwrites    :initarg :overwrites
		  :type (array overwrite)
		  :accessor overwrites)
   (parent-id     :initarg :parent-id
		  :type (or null snowflake)
		  :accessor parent-id)))

(defclass category (guild-channel) nil)

(defclass text-channel (guild-channel)
  ((nsfw         :initarg :nsfw
		 :type t
		 :accessor nsfw-p)
   (topic        :initarg :topic
		 :type string
		 :accessor topic)
   (last-message :initarg :last-message
		 :type snowflake
		 :accessor last-message)))

(defclass voice-channel (guild-channel)
  ((bitrate    :initarg :bitrate
	       :type fixnum
	       :accessor bitrate)
   (user-limit :initarg :user-limit
	       :type fixnum
	       :accessor user-limit)))

(defclass dm-channel (channel)
  ((last-message :initarg :last-message
		 :type snowflake
		 :accessor last-message)
   (recipients   :initarg :last-message
		 :type (vector user)
		 :accessor recipients)))

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
    :id "id"
    :g-id "guild_id"
    :name "name"
    :pos "position"
    :overwrites (map 'vector (curry #'from-json :overwrite)
		     (gethash "permission_overwrites" table))
    :nsfw "nsfw"
    :topic "topic"
    :last-message "last_message_id"
    :parent-id "parent_id"))

(defun %guild-voice-fj (table)
  (instance-from-table (table 'voice-channel)
    :id "id"
    :g-id "guild_id"
    :name "name"
    :pos "position"
    :overwrites (map 'vector (curry #'from-json :overwrite)
		     (gethash "permission_overwrites" table))
    :bitrate "bitrate"
    :user-limit "user_limit"
    :parent-id "parent_id"))

(defun %dm-fj (table)
  (instance-from-table (table 'dm-channel)
    :last-message "last_message_id"
    :id "id"
    :recipients "recipients"))

(defun %group-dm-fj (table)
  (instance-from-table (table 'group-dm)
    :id "id"
    :name "name"
    :recipients (map 'vector (curry #'from-json :user)
		     (gethash "recipients" table))
    :last-message "last_message_id"
    :owner "owner_id"))

(defun %guild-category-fj (table)
  (instance-from-table (table 'category)
    :id "id"
    :overwrites (map 'vector (curry #'from-json :overwrite)
		     (gethash "permission_overwrites" table))
    :name "name"
    :parent-id "parent_id"
    :nsfw "nsfw"
    :pos "position"
    :guild-id "guild_id"))

(defmethod from-json ((c (eql :channel)) (table hash-table))
  (case (gethash "type" table)
    (0 (%guild-text-fj table))
    (1 (%dm-fj table))
    (2 (%guild-voice-fj table))
    (3 (%group-dm-fj table))
    (4 (%guild-category-fj table))
    (otherwise (dprint :error "Channel type not recognised!~%This should only happen if discord creates a new channel type and lispcord wasn't updated yet~%Please file an issue at https://github.com/MegaLoler/lispcord/issues"))))


(defmethod %to-json ((gtc text-channel))
  (with-object
    (write-key-value "id" (id gtc))
    (write-key-value "guild_id" (guild-id gtc))
    (write-key-value "name" (name gtc))
    (write-key-value "permission_overwrites" (overwrites gtc))
    (write-key-value "parent_id" (parent-id gtc))
    (write-key-value "nsfw" (nsfw gtc))
    (write-key-value "topic" (topic gtc))
    (write-key-value "last_message_id" (last-message gtc))))

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


