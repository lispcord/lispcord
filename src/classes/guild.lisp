(in-package :lispcord.classes)

(defclass partial-role ()
  ((name        :initarg :name     :accessor name)
   (color       :initarg :color    :accessor color)
   (hoist       :initarg :hoist    :accessor hoistp)
   (permissions :initarg :perms   :accessor permissions)
   (mentionable :initarg :mention :accessor mentionablep)))

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

(define-converters (partial-role)
  name color
  (hoist (defaulting-writer :false))
  permissions
  (mentionable (defaulting-writer :false)))

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
                :type permissions
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

(defmethod overwrite ((c channel) (m role))
  "Returns permission overwrite for role in channel"
  (find (id m) (overwrites c) :key 'id))

(define-converters (role)
  (id 'parse-snowflake)
  name color hoist position
  (permissions 'make-permissions)
  managed
  mentionable)

(defclass member ()
  ((user          :initarg :user
                  :type user
                  :accessor user)
   (nick          :initarg :nick
                  :type (or null string)
                  :accessor nick)
   (roles         :initarg :roles
                  :type (vector role)
                  :accessor roles)
   (joined-at     :initarg :joined-at
                  :type (or null string)
                  :accessor joined-at)
   (premium-since :initarg :premium-since
                  :type (or null string)
                  :accessor premium-since)
   (deaf          :initarg :deaf
                  :type t
                  :accessor deafp)
   (mute          :initarg :mute
                  :type t
                  :accessor mutep)
   (guild-id      :initarg :gid
                  :type (or null snowflake)
                  :accessor guild-id)))

(defmethod has-permission ((m member) key &optional channel)
  (let ((base-permissions (base-permissions m)))
    (if channel
        (has-permission (compute-overwrites base-permissions m channel) key)
        (has-permission base-permissions key))))

(defmethod has-permission ((u user) key &optional channel)
  (if channel
      (if-let ((member (member u (guild channel))))
        (has-permission member key channel)
        (error "User ~S is not a member of ~S" u channel))
      (error "Global users don't have permissions. Either replace the user object with member, or specify the channel.")))

(defmethod overwrite ((c channel) (m member))
  "Returns permission overwrite for member in channel"
  (find (id (user m)) (overwrites c) :key 'id))

(define-converters (member)
  (user (caching-reader 'user))
  (nick)
  (roles (cache-vector-id-reader 'role))
  joined-at mute deaf
  (guild-id 'parse-snowflake))

(defclass presence ()
  ((user          :initarg :user
                  :type snowflake
                  :accessor user-id)
   (roles         :initarg :roles
                  :type (vector snowflake)
                  :accessor roles)
   (game          :initarg :game
                  :type (or null game)
                  :accessor game)
   (guild-id      :initarg :guild-id
                  :type snowflake
                  :accessor guild-id)
   (status        :initarg :status
                  :type (or null string)
                  :accessor status)
   (activities    :initarg :activities
                  :type (vector game)
                  :accessor activities)
   (client-status :initarg client-status
                  :type (or null client-status)
                  :accessor client-status)
   (premium-since :initarg premium-since
                  :type (or null string)
                  :accessor premium-since)
   (nick          :initarg nick
                  :type (or null string)
                  :accessor nick)))

(defmethod user ((p presence))
  (getcache-id (user-id p) :user))

(define-converters (presence)
  (user (lambda (d) (parse-snowflake (gethash "id" d))))
  (roles (vector-reader 'parse-snowflake))
  (game (subtable-reader 'game))
  (guild-id '%maybe-sf)
  status
  (activities (subtable-vector-reader 'game))
  (client-status (subtable-reader 'client-status))
  premium-since nick)

(defclass client-status ()
  ((desktop :initarg :desktop
            :type string
            :accessor desktop)
   (mobile  :initarg :mobile
            :type string
            :accessor mobile)
   (web     :initarg :web
            :type string
            :accessor web)))

(define-converters (client-status)
  desktop mobile web)

(defclass guild ()
  ((id                 :initarg :id
                       :type snowflake
                       :accessor id)
   (unavailable        :initarg :unavailable
                       :type boolean
                       :accessor unavailable)))

(define-converters (guild)
  (id 'parse-snowflake)
  (available))

(defclass unavailable-guild (guild) ())

(defclass available-guild (guild)
  ((name               :initarg :name
                       :type string
                       :accessor name)
   (icon               :initarg :icon
                       :type (or null string)
                       :accessor icon)
   (splash             :initarg :splash
                       :type (or null string)
                       :accessor splash)
   (owner              :initarg :owner
                       :type snowflake
                       :accessor owner-id)
   (region             :initarg :region
                       :type string
                       :accessor region)
   (afk-id             :initarg :afk-id
                       :type (or null snowflake)
                       :accessor afk-id)
   (afk-timeout        :initarg :afk-timeout
                       :type fixnum
                       :accessor afk-timeout)
   (embed?             :initarg :embed?
                       :type t
                       :accessor embedp)
   (embed-id           :initarg :embed-id
                       :type (or null snowflake)
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
                       :type (or null (vector string))
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
                       :type (or null snowflake)
                       :accessor widget-id)
   (system-channel-id  :initarg :system-id
                       :type (or null snowflake)
                       :accessor system-channel-id)
   (joined-at          :initarg :joined-at
                       :type (or null string)
                       :accessor joined-at)
   (large              :initarg :large
                       :type t
                       :accessor largep)
   (member-count       :initarg :member-cnt
                       :type (or null fixnum)
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

(defmethod role-everyone ((g guild))
  "Returns the @everyone role of the guild"
  ;; It always has the same id as the guild
  (getcache-id (id g) :role))

(defmethod member ((u user) (g guild))
  (find-if (lambda (e) (eq u (lc:user e))) (members g)))

(defmethod nick-or-name ((u user) (g  guild))
  "Member u of the guild g"
  (if-let ((member (member u g)))
    (or (nick member)
        (name u))
    (name u)))

;; Deprecated accessors

(defmethod availablep ((g guild))
  (not (unavailable g)))

(defmethod afk-to ((g guild))
  (afk-timeout g))

;; End of deprecated accessors

(define-converters (available-guild)
  name icon splash region
  (owner 'parse-snowflake)
  afk-timeout
  (afk-channel-id '%maybe-sf)
  embed-enabled
  (embed-channel-id '%maybe-sf)
  verification-level
  default-message-notifications
  explicit-content-filter
  (roles (caching-vector-reader 'role))
  (emojis (caching-vector-reader 'emoji))
  (features (lambda (v) (coerce v 'vector)))
  mfa-level
  (application-id '%maybe-sf)
  widget-enabled
  (widget-channel-id '%maybe-sf)
  (system-channel-id '%maybe-sf)
  joined-at large member-count
  (members (subtable-vector-reader 'member))
  (channels (caching-vector-reader 'channel))
  (presences (subtable-vector-reader 'presence)))

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
      (dolist* (obj (roles emojis members channels))
        (unless (guild-id obj)
          (setf (guild-id obj) gid))))
    obj))
