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

(defclass guild-member ()
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
(defmethod %to-json ((m guild-member))
  (with-object
    (write-key-value "user" (!! m user))
    (write-key-value "nick" (!! m nick))
    (write-key-value "roles" (!! m roles))
    (write-key-value "joined_at" (!! m joined-at))
    (write-key-value "mute" (!! m mute))
    (write-key-value "deaf" (!! m deaf))))

(defmethod from-json ((c (eql :guild-member)) (table hash-table))
  (instance-from-table (table 'guild-member)
    :user "user"
    :nick "nick"
    :roles "roles"
    :joined-at "joined_at"
    :mute "mute"
    :deaf "deaf"))


(defclass guild ()
  ((id                 :initarg :id         :type snowflake)
   (name               :initarg :name       :type string)
   (icon               :initarg :icon       :type string)
   (splash             :initarg :splash     :type string)
   (owner              :initarg :owner      :type snowflake)
   (region             :initarg :region     :type string)
   (afk-id             :initarg :afk-id     :type snowflake)
   (afk-to             :initarg :afk-to     :type fixnum)
   (embed?             :initarg :embed?     :type t)
   (embed-id           :initarg :embed-id   :type snowflake)
   (verification-level :initarg :verify-l   :type fixnum)
   (notification-level :initarg :notify-l   :type fixnum)
   (content-filter     :initarg :content    :type fixnum)
   (roles              :initarg :roles      :type (vector role))
   (emojis             :initarg :emojis     :type (vector emoji))
   (features           :initarg :features   :type (vector string))
   (mfa-level          :initarg :mfa        :type fixnum)
   (application-id     :initarg :app-id     :type (or null snowflake))
   (widget-enabled     :initarg :widget?    :type t)
   (widget-channel-id  :initarg :widget-id  :type snowflake)
   (joined-at          :initarg :joined-at  :type string)
   (large              :initarg :large      :type t)
   (unavailable        :initarg :available  :type t)
   (member-count       :initarg :member-cnt :type fixnum)
   (members            :initarg :members    :type (vector guild-member))
   (channels           :initarg :channels   :type (vector channel))
   (presences          :initarg :presences  :type (vector presence))))

(defmethod %to-json ((g guild))
  (with-object
    (write-key-value "id" (!! g id))
    (write-key-value "name" (!! g name))
    (write-key-value "icon" (!! g icon))
    (write-key-value "joined_at" (!! g joined-at))
    (write-key-value "splash" (!! g splash))
    (write-key-value "owner_id" (!! g owner))
    (write-key-value "region" (!! g region))
    (write-key-value "afk_channel_id" (!! g afk-id))
    (write-key-value "afk_timeout" (!! g afk-to))
    (write-key-value "embed_enabled" (!! g embed?))
    (write-key-value "embed_channel_id" (!! g embed-id))
    (write-key-value "verification_level" (!! g verification-level))
    (write-key-value "default_message_notification" (!! g notification-leve))
    (write-key-value "explicit_content_filter" (!! g content-filter))
    (write-key-value "roles" (!! g roles))
    (write-key-value "emojis" (!! g emojis))
    (write-key-value "features" (!! g features))
    (write-key-value "mfa_level" (!! g mfa-level))
    (write-key-value "application_id" (!! g application-id))
    (write-key-value "widget_enabled" (!! g widget-enabled))
    (write-key-value "widget_channel_id" (!! g widget-channel-id))
    (write-key-value "large" (!! g large))
    (write-key-value "unavailable" (!! g unavailable))
    (write-key-value "member_count" (!! g member-count))
    (write-key-value "members" (!! g members))
    (write-key-value "channels" (!! g channels))
    (write-key-value "presences" (!! g presences))))

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
    :available "unavailable"
    :member-cnt "member_count"
    :members (map 'vector (curry #'from-json :g-member)
		  (gethash "members" table))
    :channels (map 'vector (curry #'from-json :channel)
		   (gethash "channels" table))
    :presences (map 'vector (curry #'from-json :presence)
		    (gethash "presences" table))))
