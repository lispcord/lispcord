(in-package :lispcord.classes)


(defclass role ()
  ((id          :initarg :id
		:type snowflake
		:accessor id)
   (name        :initarg :name
		:type string
		:accessor name)
   (color       :initarg :color
		:type fixnum
		:accessor color)
   (hoist       :initarg :hoist
		:type t
		:accessor hoistp)
   (position    :initarg :pos
		:type fixnum
		:accessor position)
   (permissions :initarg :perms
		:type fixnum
		:accessor permissions)
   (managed     :initarg :managed
		:type t
		:accessor managedp)
   (mentionable :initarg :mentionable
		:type t
		:accessor mentionablep)
   (guild-id    :initarg :gid
		:type (or null snowflake)
		:accessor guild-id)))

(defmethod from-json ((c (eql :role)) (table hash-table))
  (instance-from-table (table 'role)
		       :id "id"
		       :name "name"
		       :color "color"
		       :hoist "hoist"
		       :pos "position"
		       :perms "permissions"
		       :managed "managed"
		       :mentionable "mentionable"
		       :gid "guild_id"))

(defmethod %to-json ((r role))
  (with-object
      (write-key-value "id" (id r))
    (write-key-value "name" (name r))
    (write-key-value "color" (color r))
    (write-key-value "hoist" (hoistp r))
    (write-key-value "position" (position r))
    (write-key-value "permissions" (permissions r))
    (write-key-value "managed" (managedp r))
    (write-key-value "mentionable" (mentionablep r))))

(defclass member ()
  ((user      :initarg :user
	      :type user
	      :accessor user)
   (nick      :initarg :nick
	      :type (or null string)
	      :accessor nick)
   (roles     :initarg :roles
	      :type (vector role)
	      :accessor roles)
   (joined-at :initarg :joined-at
	      :type (or null string)
	      :accessor joined-at)
   (deaf      :initarg :deaf
	      :type t
	      :accessor deafp)
   (mute      :initarg :mute
	      :type t
	      :accessor mutep)
   (guild-id  :initarg :gid
	      :type (or null snowflake)
	      :accessor guild-id)))

;;The Modify and Add Member REST-calls can use this
(defmethod %to-json ((m member))
  (with-object
    (write-key-value "user" (user m))
    (write-key-value "nick" (nick m))
    (write-key-value "roles" (roles m))
    (write-key-value "joined_at" (joined-at m))
    (write-key-value "mute" (mutep m))
    (write-key-value "deaf" (deafp m))))

(defmethod from-json ((c (eql :g-member)) (table hash-table))
  (instance-from-table (table 'member)
    :user "user"
    :nick "nick"
    :roles "roles"
    :joined-at "joined_at"
    :mute "mute"
    :deaf "deaf"
    :gid "guild_id"))


(defclass game ()
  ((name :initarg :name
	 :type string
	 :accessor name)
   (type :initarg :type
	 :type (integer 0 1)
	 :accessor type)
   (url  :initarg :url
	 :type (or null string)
	 :accessor url)))

(defmethod from-json ((c (eql :game)) (table hash-table))
  (instance-from-table (table 'game)
    :name "name"
    :type "type"
    :url "url"))

(defmethod %to-json ((g game))
  (with-object
    (write-key-value "name" (name g))
    (write-key-value "type" (type g))
    (write-key-value "url" (url g))))

(defun make-game (game-name &optional (type 0) (url nil))
  (make-instance 'game :name game-name :type type :url url))

(defclass presence ()
  ((user     :initarg :user
	     :type snowflake
	     :accessor user)
   (roles    :initarg :roles
	     :type (or null (vector role))
	     :accessor roles)
   (game     :initarg :game
	     :type (or null game)
	     :accessor game)
   (guild-id :initarg :guild-id
	     :type (or null snowflake)
	     :accessor guild-id)
   (status   :initarg :status
	     :type (or null string)
	     :accessor status)))

(defmethod from-json ((c (eql :presence)) (table hash-table))
  (instance-from-table (table 'presence)
    :user (gethash "id" (gethash "user" table))
    :roles (map 'vector (curry #'from-json :role)
		(gethash "roles" table))
    :game (from-json :game (gethash "game" table))
    :guild-id "guild_id"
    :status "status"))

(defmethod %to-json ((p presence))
  (with-object
    (write-key-value "name" (name p))
    (write-key-value "roles" (roles p))
    (write-key-value "game" (game p))
    (write-key-value "guild_id" (guild-id p))
    (write-key-value "status" (status p))))



(defclass guild (guild-object)
  ((id                 :initarg :id
		       :type snowflake
		       :accessor id)
   (name               :initarg :name
		       :type string
		       :accessor name)
   (icon               :initarg :icon
		       :type string
		       :accessor icon)
   (splash             :initarg :splash
		       :type string
		       :accessor splash)
   (owner              :initarg :owner
		       :type snowflake
		       :accessor owner)
   (region             :initarg :region
		       :type string
		       :accessor region)
   (afk-id             :initarg :afk-id
		       :type snowflake
		       :accessor afk-id)
   (afk-to             :initarg :afk-to
		       :type fixnum
		       :accessor afk-to)
   (embed?             :initarg :embed?
		       :type t
		       :accessor embedp)
   (embed-id           :initarg :embed-id
		       :type snowflake
		       :accessor embed-id)
   (verification-level :initarg :verify-l
		       :type fixnum
		       :accessor verify-level)
   (notification-level :initarg :notify-l
		       :type fixnum
		       :accessor notify-level)
   (content-filter     :initarg :content
		       :type fixnum
		       :accessor content-filter)
   (roles              :initarg :roles
		       :type (vector role)
		       :accessor roles)
   (emojis             :initarg :emojis
		       :type (vector emoji)
		       :accessor emojis)
   (features           :initarg :features
		       :type (vector string)
		       :accessor features)
   (mfa-level          :initarg :mfa
		       :type fixnum
		       :accessor mfa-level)
   (application-id     :initarg :app-id
		       :type (or null snowflake)
		       :accessor app-id)
   (widget-enabled     :initarg :widget?
		       :type t
		       :accessor widgetp)
   (widget-channel-id  :initarg :widget-id
		       :type snowflake
		       :accessor widget-id)
   (joined-at          :initarg :joined-at
		       :type string
		       :accessor joined-at)
   (large              :initarg :large
		       :type t
		       :accessor largep)
   (available          :initarg :available
		       :type t
		       :accessor availablep)
   (member-count       :initarg :member-cnt
		       :type fixnum
		       :accessor member-count)
   (members            :initarg :members
		       :type (vector member)
		       :accessor members)
   (channels           :initarg :channels
		       :type (vector channel)
		       :accessor channels)
   (presences          :initarg :presences
		       :type (vector presence)
		       :accessor presences)))

(defmethod %to-json ((g guild))
  (with-object
    (write-key-value "id" (id g))
    (write-key-value "name" (name g))
    (write-key-value "icon" (icon g))
    (write-key-value "joined_at" (joined-at g))
    (write-key-value "splash" (splash g))
    (write-key-value "owner_id" (owner g))
    (write-key-value "region" (region g))
    (write-key-value "afk_channel_id" (afk-id g))
    (write-key-value "afk_timeout" (afk-to g))
    (write-key-value "embed_enabled" (embedp g))
    (write-key-value "embed_channel_id" (embed-id g))
    (write-key-value "verification_level" (verify-level g))
    (write-key-value "default_message_notification" (notify-level g))
    (write-key-value "explicit_content_filter" (content-filter g))
    (write-key-value "roles" (roles g))
    (write-key-value "emojis" (emojis g))
    (write-key-value "features" (features g))
    (write-key-value "mfa_level" (mfa-level g))
    (write-key-value "application_id" (app-id g))
    (write-key-value "widget_enabled" (widgetp g))
    (write-key-value "widget_channel_id" (widget-id g))
    (write-key-value "large" (largep g))
    (write-key-value "unavailable" (not (availablep g)))
    (write-key-value "member_count" (member-count g))
    (write-key-value "members" (members g))
    (write-key-value "channels" (channels g))
    (write-key-value "presences" (presences g))))

(defmethod from-json ((c (eql :class)) (table hash-table))
  (instance-from-table (table 'guild)
    :id "id"
    :name "name"
    :icon "icon"
    :splash "splash"
    :owner "owner_id"
    :region "region"
    :afk-id "afk_channel_id"
    :afk-to "afk_timeout"
    :embed? "embed_enabled"
    :embed-id "embed_channel_id"
    :verify-l "verification_level"
    :notify-l "default_message_notifications"
    :content "explicit_content_filter"
    :roles (map 'vector (curry #'from-json :role)
		(gethash "roles" table))
    :emojis (map 'vector (curry #'from-json :emoji)
		 (gethash "emojis" table))
    :features (coerce (gethash "features" table) '(vector string))
    :mfa "mfa_level"
    :app-id "application_id"
    :widget? "widget_enabled"
    :widget-id "widget_channel_id"
    :joined-at "joined_at"
    :large "large"
    :available (not (gethash "unavailable" table))
    :member-cnt "member_count"
    :members (map 'vector (curry #'from-json :g-member)
		  (gethash "members" table))
    :channels (map 'vector (curry #'from-json :channel)
		   (gethash "channels" table))
    :presences (map 'vector (curry #'from-json :presence)
		    (gethash "presences" table))))
