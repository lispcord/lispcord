(in-package :lispcord.classes.channel)

(deftype overwrite-type ()
  `(and string
	(satisfies ,(lambda (s)
		      (or (string= s "member") (string= s "role"))))))

(defclass overwrite ()
  ((id    :initarg :id
	  :type snowflake)
   (type  :initarg :type
	  :type overwrite-type)
   (allow :initarg :allow
	  :type fixnum)
   (deny  :initarg :deny
	  :type fixnum)))

(defmethod from-json ((c (eql :overwrite)) (table hash-table))
  (instance-from-table (table 'overwrite)
    :id "id"
    :type "type"
    :allow "allow"
    :deny "deny"))

(defmethod %to-json ((o overwrite))
  (with-object
    (write-key-value "id" (!! o id))
    (write-key-value "type" (!! o type))
    (write-key-value "allow" (!! o allow))
    (write-key-value "deny" (!! o deny))))

(defclass channel ()
  ((id :initarg :id
       :type snowflake)))

(defclass guild-channel (channel)
  ((guild-id      :initarg :g-id
		  :type snowflake)
   (name          :initarg :name
		  :type string)
   (position      :initarg :pos
		  :type fixnum)
   (overwrites    :initarg :overwrites
		  :type (array overwrite))
   (parent-id     :initarg :parent-id
		  :type (or null snowflake))))

(defclass category (guild-channel) ())

(defclass text-channel (guild-channel)
  ((nsfw         :initarg :nsfw
		 :type t)
   (topic        :initarg :topic
		 :type string)
   (last-message :initarg :last-message
		 :type snowflake)))

(defclass voice-channel (guild-channel)
  ((bitrate    :initarg :bitrate
	       :type fixnum)
   (user-limit :initarg :user-limit
	       :type fixnum)))

(defclass dm-channel (channel)
  ((last-message :initarg :last-message
		 :type snowflake)
   (recipients   :initarg :last-message
		 :type (vector user))))

(defclass group-dm (dm-channel)
  ((name     :initarg :name
	     :type string)
   (icon     :initarg :icon
	     :type (or null string))
   (owner    :initarg :owner
	     :type snowflake)))

(defun %guild-text-fj (table)
  (instance-from-table (table 'text-channel)
    :id "id"
    :guild-id "guild_id"
    :name "name"
    :position "position"
    :overwrites (map 'vector (curry #'from-json :overwrite)
		     (gethash "permission_overwrites" table))
    :nsfw "nsfw"
    :topic "topic"
    :last-message "last_message_id"
    :parent-id "parent_id"))

(defun %guild-voice-fj (table)
  (instance-from-table (table 'voice-channel)
    :id "id"
    :guild-id "guild_id"
    :name "name"
    :position "position"
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
    :position "position"
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
    (write-key-value "id" (!! gtc id))
    (write-key-value "guild_id" (!! gtc guild-id))
    (write-key-value "name" (!! gtc name))
    (write-key-value "permission_overwrites" (!! gtc overwrites))
    (write-key-value "parent_id" (!! gtc parent-id))
    (write-key-value "nsfw" (!! gtc nsfw))
    (write-key-value "topic" (!! gtc topic))
    (write-key-value "last_message_id" (!! gtc last-message))))

(defmethod %to-json ((gvc voice-channel))
  (with-object
    (write-key-value "id" (!! gvc id))
    (write-key-value "guild_id" (!! gvc guild-id))
    (write-key-value "name" (!! gvc name))
    (write-key-value "permission_overwrites" (!! gvc overwrites))
    (write-key-value "parent_id" (!! gvc parent-id))
    (write-key-value "bitrate" (!! gvc bitrate))
    (write-key-value "user_limit" (!! gvc user-limit))))

(defmethod %to-json ((dm dm-channel))
  (with-object
    (write-key-value "id" (!! dm id))
    (write-key-value "last_message_id" (!! dm last-message))
    (write-key-value "recipients" (!! dm recipients))))

(defmethod %to-json ((gdm group-dm))
  (with-object
    (write-key-value "id" (!! gdm id))
    (write-key-value "name" (!! gdm name))
    (write-key-value "icon" (!! gdm icon))
    (write-key-value "owner_id" (!! gdm owner))))


