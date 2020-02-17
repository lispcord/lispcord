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

(define-converters (partial-role)
  name color
  (hoist (defaulting-writer :false))
  permissions
  (mentionable (defaulting-writer :false)))

(defclass* role ()
  ((id          :type snowflake)
   (name        :type string)
   (color       :type fixnum)
   (hoist       :type boolean :accessor hoistp)
   (position    :type fixnum)
   (permissions :type permissions)
   (managed     :type boolean :accessor managedp)
   (mentionable :type boolean :accessor mentionablep)
   (guild-id    :type (or null snowflake))))

(defmethod overwrite ((c channel) (m role))
  "Returns permission overwrite for role in channel"
  (find (id m) (overwrites c) :key 'id))

(define-converters (role)
  (id 'parse-snowflake)
  name color hoist position
  (permissions 'make-permissions)
  managed
  mentionable)

(defclass* member ()
  ((user          :type user)
   (nick          :type (or null string))
   (roles         :type (vector role))
   (joined-at     :type (or null string))
   (premium-since :type (or null string))
   (deaf          :type boolean :accessor deafp)
   (mute          :type boolean :accessor mutep)
   (guild-id      :type (or null snowflake))))

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

(defclass* presence ()
  ((user          :type snowflake)
   (roles         :type (vector snowflake))
   (game          :type (or null game))
   (guild-id      :type snowflake)
   (status        :type (or null string))
   (activities    :type (vector game))
   (client-status :type (or null client-status))
   (premium-since :type (or null string))
   (nick          :type (or null string))))

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

(defclass* client-status ()
  ((desktop :type string)
   (mobile  :type string)
   (web     :type string)))

(define-converters (client-status)
  desktop mobile web)

(defclass* guild ()
  ((id          :type snowflake)
   (unavailable :type boolean :accessor unavailablep)))

(define-converters (guild)
  (id 'parse-snowflake)
  (unavailable))

(defclass* unavailable-guild (guild) ())

(defclass* available-guild (guild)
  ((name :type string)
   (icon :type (or null string))
   (splash :type (or null string))
   (owner :type boolean :accessor ownerp)
   (owner-id :type snowflake)
   (region :type string)
   (afk-id :type (or null snowflake))
   (afk-timeout :type fixnum)
   (embed :type boolean :accessor embedp)
   (embed-id :type (or null snowflake))
   (verification-level :type fixnum)
   (notification-level :type fixnum)
   (content-filter :type fixnum)
   (roles :type (vector role))
   (emojis :type (vector emoji))
   (features :type (or null (vector string)))
   (mfa-level :type fixnum)
   (application-id :type (or null snowflake))
   (widget-enabled :type boolean :accessor widget-enabledp)
   (widget-channel-id :type (or null snowflake))
   (system-channel-id :type (or null snowflake))
   (joined-at :type (or null string))
   (large :type boolean :accessor largep)
   (member-count :type (or null fixnum))
   (members :type (vector member))
   (channels :type (vector channel))
   (presences :type (vector presence))))

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
