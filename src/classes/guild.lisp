(in-package :lispcord.classes)

(defclass* partial-role ()
  ((name)
   (color)
   (hoist)
   (permissions)
   (mentionable)))

(export-pub make-role)
(defmethod make-role (&key
                        (name "new role")
                        (color 0)
                        hoist
                        (permissions 0)
                        mentionable)
  (make-instance 'partial-role
                 :name name
                 :color color
                 :hoist (or hoist :false)
                 :perms permissions
                 :mention (or mentionable :false)))

(define-converters (%to-json) partial-role
  name color
  (hoist (defaulting-writer :false))
  permissions
  (mentionable (defaulting-writer :false)))

(defclass* role ()
  ((id          :type snowflake)
   (name        :type string)
   (color       :type fixnum)
   (hoist       :type boolean :accessor hoist-p)
   (position    :type fixnum)
   (permissions :type permissions)
   (managed     :type boolean :accessor managed-p)
   (mentionable :type boolean :accessor mentionable-p)
   (guild-id    :type (or null snowflake))))

(define-converters (%to-json from-json) role
  (id 'parse-snowflake)
  name color
  (hoist nil (defaulting-writer :false))
  position
  (permissions 'make-permissions)
  managed
  mentionable
  (guild-id :ignore :ignore))

(defclass* member ()
  ((user          :type user)
   (nick          :type (or null string))
   (roles         :type (vector role))
   (joined-at     :type (or null string))
   (premium-since :type (or null string))
   (deaf          :type boolean :accessor deaf-p)
   (mute          :type boolean :accessor mute-p)
   (guild-id      :type (or null snowflake))))

(define-converters (%to-json from-json) member
  (user (caching-reader 'user))
  (nick)
  (roles (cache-vector-id-reader 'role))
  premium-since joined-at
  (mute nil (defaulting-writer :false))
  (deaf nil (defaulting-writer :false))
  (guild-id :ignore :ignore))

(defclass* presence ()
  ((user          :type user)
   (roles         :type (vector snowflake))
   (game          :type (or null activity))
   (guild-id      :type (or null snowflake))
   (status        :type string)
   (activities    :type (vector activity))
   (client-status :type (or null client-status))
   (premium-since :type (or null string))
   (nick          :type (or null string))))

(defmethod user ((p presence))
  (getcache-id (user-id p) :user))

(define-converters (%to-json from-json) presence
  (user (lambda (d) (getcache-id (parse-snowflake (gethash "id" d)) 'user)))
  (roles (vector-reader 'parse-snowflake))
  (game (subtable-reader 'activity))
  (guild-id '%maybe-sf)
  status
  (activities (subtable-vector-reader 'activity))
  (client-status (subtable-reader 'client-status))
  premium-since nick)

(defclass* client-status ()
  ((desktop :type string)
   (mobile  :type string)
   (web     :type string)))

(define-converters (%to-json from-json) client-status
  desktop mobile web)

(defclass* base-guild ()
  ((id          :type snowflake)
   (unavailable :type boolean :accessor unavailable-p)))

(define-converters (%to-json from-json) base-guild
  (id 'parse-snowflake)
  (unavailable nil (defaulting-writer :false)))

(defclass* unavailable-guild (base-guild) ())

(define-converters (%to-json from-json) unavailable-guild)

(defclass* available-guild (base-guild)
  ((name :type string)
   (icon :type (or null string))
   (splash :type (or null string))
   (owner :type boolean :accessor owner-p)
   (owner-id :type snowflake)
   (permissions :type (or null permissions))
   (region :type string)
   (afk-channel-id :type (or null snowflake))
   (afk-timeout :type fixnum)
   (embed-enabled :type boolean :accessor embed-enabled-p)
   (embed-channel-id :type (or null snowflake))
   (verification-level :type fixnum)
   (default-message-notifications :type fixnum)
   (explicit-content-filter :type fixnum)
   (roles :type (vector role))
   (emojis :type (vector emoji))
   (features :type (vector string))
   (mfa-level :type fixnum)
   (application-id :type (or null snowflake))
   (widget-enabled :type boolean :accessor widget-enabled-p)
   (widget-channel-id :type (or null snowflake))
   (system-channel-id :type (or null snowflake))
   (joined-at :type (or null string))
   (large :type boolean :accessor large-p)
   (member-count :type (or null fixnum))
   (voice-states :type (vector voice-state))
   (members :type (vector member))
   (channels :type (vector channel))
   (presences :type (vector presence))
   (max-presences :type (or null fixnum))
   (max-members :type (or null fixnum))
   (vanity-url-code :type (or null string))
   (description :type (or null string))
   (banner :type (or null string))
   (premium-tier :type fixnum)
   (premium-subscription-count :type (or null fixnum))
   (preferred-locale :type string)))

(define-converters (%to-json from-json) available-guild
  (name)
  (icon)
  (splash)
  (owner nil (defaulting-writer :false))
  (owner-id 'parse-snowflake)
  (permissions 'make-permissions)
  (region)
  (afk-channel-id '%maybe-sf)
  (afk-timeout)
  (embed-enabled nil (defaulting-writer :false))
  (embed-channel-id '%maybe-sf)
  (verification-level)
  (default-message-notifications)
  (explicit-content-filter)
  (roles (caching-vector-reader 'role))
  (emojis (caching-vector-reader 'emoji))
  (features (subtable-reader 'guild-feature))
  (mfa-level)
  (application-id '%maybe-sf)
  (widget-enabled nil (defaulting-writer :false))
  (widget-channel-id '%maybe-sf)
  (system-channel-id '%maybe-sf)
  (joined-at)
  (large nil (defaulting-writer :false))
  (member-count)
  (voice-states (subtable-vector-reader 'voice-state))
  (members (subtable-vector-reader 'member))
  (channels (caching-vector-reader 'channel))
  (presences (subtable-vector-reader 'presence))
  (max-presences)
  (max-members)
  (vanity-url-code)
  (description)
  (banner)
  (premium-tier)
  (premium-subscription-count)
  (preferred-locale))

(export-pub guild)
(defmethod from-json ((c (eql 'guild)) (table hash-table))
  (if (gethash "unavailable" table)
      (from-json 'unavailable-guild table)
      (from-json 'available-guild table)))

(defmethod from-json :around ((c (eql 'available-guild)) (table hash-table))
  "Remember guild-id for roles, emojis, members, and channels"
  (let ((obj (call-next-method))
        (gid (parse-snowflake (gethash "id" table))))
    (with-slots (roles emojis members channels)
        obj
      (dovec* (obj (roles emojis members channels))
        (unless (and (slot-boundp obj 'guild-id)
                     (guild-id obj))
          (setf (guild-id obj) gid))))
    obj))

(defclass* ban ()
  ((reason :type (or null string))
   (user :type user)))

(define-converters (%to-json from-json) ban
  (reason)
  (user (caching-reader 'user)))
