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

(defun make-overwrite (id &optional (allow 0) (deny 0) (type "role"))
  (make-instance 'overwrite :id id :allow allow :deny deny :type type))

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


(defclass chnl nil nil)

(defclass partial-channel (chnl)
  ((name       :initarg :name       :accessor name)
   (position   :initarg :pos        :accessor position)
   (topic      :initarg :topic      :accessor topic)
   (nsfw       :initarg :nsfw       :accessor nsfw-p)
   (bitrate    :initarg :bitrate    :accessor bitrate)
   (user-lim   :initarg :user-lim   :accessor user-limit)
   (overwrites :initarg :overwrites :accessor overwrites)
   (parent-id  :initarg :parent     :accessor parent-id)
   (type       :initarg :type       :accessor type)))

(defun make-channel (&key name position topic nsfw
		       bitrate user-limit overwrites
		       parent-id type)
  (make-instance 'partial-channel
		 :name name
		 :pos position
		 :topic topic
		 :nsfw nsfw
		 :bitrate bitrate
		 :user-lim user-limit
		 :overwrites overwrites
		 :parent parent-id
		 :type type))

(defmethod %to-json ((c chnl))
  (let ((name (name c))
	(pos (position c))
	(top (topic c))
	(nsfw (nsfw-p c))
	(bit (bitrate c))
	(lim (user-limit c))
	(ovw (overwrites c))
	(parent (parent-id c))
	(type (type c)))
    (with-object
      (if name (write-key-value "name" name))
      (if pos (write-key-value "position" pos))
      (if top (write-key-value "topic" top))
      (if nsfw (write-key-value "nsfw" nsfw))
      (if bit (write-key-value "bitrate" bit))
      (if lim (write-key-value "user_limit" lim))
      (if ovw (write-key-value "permission_overwrites"ovw))
      (if parent (write-key-value "parent_id" parent))
      (if type (write-key-value "type" type)))))




(defclass channel (chnl)
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
   (owner-id :initarg :owner
	     :type snowflake
	     :accessor owner-id)))

(defmethod guild ((c guild-channel))
  (getcache-id (guild-id c) :guild))

(defmethod parent ((c guild-channel))
  (getcache-id (parent-id c) :channel))

(defmethod owner ((c group-dm))
  (getcache-id (owner-id c) :user))

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
    :last-message (%maybe-sf (gethash "last_message_id" table))
    :parent-id (%maybe-sf (gethash "parent_id" table))
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
    :parent-id (%maybe-sf (gethash "parent_id" table))))

(defun %dm-fj (table)
  (instance-from-table (table 'dm-channel)
    :last-message (%maybe-sf (gethash "last_message_id" table))
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
    :last-message (%maybe-sf (gethash "last_message_id" table))
    :owner (parse-snowflake (gethash "owner_id" table))
    :last-pinned "last_pin_timestamp"))

(defun %guild-category-fj (table)
  (instance-from-table (table 'category)
    :id (parse-snowflake (gethash "id" table))
    :overwrites (mapvec (curry #'from-json :overwrite)
		     (gethash "permission_overwrites" table))
    :name "name"
    :parent-id (%maybe-sf (gethash "parent_id" table))
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
    ("owner_id" (owner-id c) (parse-snowflake data))
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


