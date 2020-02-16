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

(defmethod %to-json ((r partial-role))
  (with-object
    (write-key-value "name" (name r))
    (write-key-value "color" (color r))
    (write-key-value "hoist" (or (hoistp r) :false))
    (write-key-value "permissions" (permissions r))
    (write-key-value "mentionable" (or (mentionablep r) :false))))

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

(defmethod guild ((r role))
  (getcache-id (guild-id r) :guild))

(defmethod overwrite ((c channel) (m role))
  "Returns permission overwrite for role in channel"
  (find (id m) (overwrites c) :key 'id))

(defmethod from-json ((c (eql 'role)) (table hash-table))
  (instance-from-table (table 'role)
                       :id (parse-snowflake (gethash "id" table))
                       :name "name"
                       :color "color"
                       :hoist "hoist"
                       :pos "position"
                       :perms (make-permissions (gethash "permissions" table))
                       :managed "managed"
                       :mentionable "mentionable"))

(defmethod update ((table hash-table) (r role))
  (from-table-update (table data)
                     ("id" (id r) (parse-snowflake data))
                     ("name" (name r) data)
                     ("color" (color r) data)
                     ("hoist" (hoistp r) data)
                     ("position" (position r) data)
                     ("permissions" (permissions r) data)
                     ("managed" (managedp r) data)
                     ("mentionable" (mentionablep r) data))
  r)

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

(defmethod guild ((m member))
  (getcache-id (guild-id m) :guild))

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

;;The Modify and Add Member REST-calls can use this
(defmethod %to-json ((m member))
  (with-object
    (write-key-value "user" (user m))
    (write-key-value "nick" (nick m))
    (write-key-value "roles" (roles m))
    (write-key-value "joined_at" (joined-at m))
    (write-key-value "mute" (mutep m))
    (write-key-value "deaf" (deafp m))))

(defmethod from-json ((c (eql 'member)) (table hash-table))
  (instance-from-table (table 'member)
                       :user (cache :user (gethash "user" table))
                       :nick "nick"
                       :roles (mapvec (lambda (e) (getcache-id (parse-snowflake e) :role))
                                      (gethash "roles" table))
                       :joined-at "joined_at"
                       :mute "mute"
                       :deaf "deaf"
                       :gid (parse-snowflake (gethash "guild_id" table))))

(defmethod update ((table hash-table) (m member))
  (from-table-update (table data)
                     ("user" (user m) (cache :user data))
                     ("nick" (nick m) data)
                     ("roles" (roles m) (mapvec #'parse-snowflake data))
                     ("joined_at" (joined-at m) data)
                     ("mute" (mutep m) data)
                     ("deaf" (deafp m) data)
                     ("guild_id" (guild-id m) (parse-snowflake data)))
  m)

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

(defmethod from-json ((c (eql 'presence)) (table hash-table))
  (instance-from-table (table 'presence)
                       :user (parse-snowflake (gethash "id" (gethash "user" table)))
                       :roles (mapvec #'parse-snowflake (gethash "roles" table))
                       :game (from-json 'game (gethash "game" table))
                       :guild-id (%maybe-sf (gethash "guild_id" table))
                       :status "status"
                       :activities (mapvec (lambda (e) (from-json 'game e))
                                           (gethash "activities" table))
                       :client-status (from-json 'client-status (gethash "client_status" table))
                       :premium-since "premium_since"
                       :nick "nick"))

(defmethod %to-json ((p presence))
  (with-object
    (write-key-value "name" (name p))
    (write-key-value "roles" (roles p))
    (write-key-value "game" (game p))
    (write-key-value "guild_id" (guild-id p))
    (write-key-value "status" (status p))
    (write-key-value "activities" (activities p))
    (write-key-value "client_status" (client-status p))
    (write-key-value "nick" (nick p))))

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

(defmethod from-json ((c (eql 'client-status)) (table hash-table))
  (instance-from-table (table 'presence)
                       :desktop "desktop"
                       :mobile  "mobile"
                       :web     "web"))

(defmethod %to-json ((cs client-status))
  (with-object
    (write-key-value "desktop" (desktop cs))
    (write-key-value "mobile"  (mobile cs))
    (write-key-value "web"     (web cs))))


(defclass guild ()
  ((id                 :initarg :id
                       :type snowflake
                       :accessor id)
   (available          :initarg :available
                       :type t
                       :accessor availablep)))

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
   (afk-to             :initarg :afk-to
                       :type fixnum
                       :accessor afk-to)
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

(defmethod owner ((g guild))
  (getcache-id (owner-id g) :user))

(defmethod member ((u user) (g guild))
  (find-if (lambda (e) (eq u (lc:user e))) (members g)))

(defmethod nick-or-name ((u user) (g  guild))
  "Member u of the guild g"
  (if-let ((member (member u g)))
    (or (nick member)
        (name u))
    (name u)))

(defmethod %to-json ((g available-guild))
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
    (write-key-value "system_channel_id" (system-channel-id g))
    (write-key-value "large" (largep g))
    (write-key-value "unavailable" (not (availablep g)))
    (write-key-value "member_count" (member-count g))
    (write-key-value "members" (members g))
    (write-key-value "channels" (channels g))
    (write-key-value "presences" (presences g))))

(defmethod %to-json ((g guild))
  (with-object
    (write-key-value "id" (id g))
    (write-key-value "unvailable" (not (availablep g)))))


(defun %available-from-json (table)
  (flet ((parse-with-gid (f type e)
           (unless (gethash "guild_id" e)
             (setf (gethash "guild_id" e) (gethash "id" table)))
           (funcall f type e)))
    (instance-from-table (table 'available-guild)
                         :id (parse-snowflake (gethash "id" table))
                         :name "name"
                         :icon "icon"
                         :splash "splash"
                         :owner (parse-snowflake (gethash "owner_id" table))
                         :region "region"
                         :afk-id (%maybe-sf (gethash "afk_channel_id" table))
                         :afk-to "afk_timeout"
                         :embed? "embed_enabled"
                         :embed-id (%maybe-sf (gethash "embed_channel_id" table))
                         :verify-l "verification_level"
                         :notify-l "default_message_notifications"
                         :content "explicit_content_filter"
                         :roles (mapvec (curry #'parse-with-gid #'cache :role)
                                        (gethash "roles" table))
                         :emojis (mapvec (curry #'parse-with-gid #'cache :emoji)
                                         (gethash "emojis" table))
                         :features (coerce (gethash "features" table) 'vector)
                         :mfa "mfa_level"
                         :app-id (%maybe-sf (gethash "application_id" table))
                         :widget? "widget_enabled"
                         :widget-id (%maybe-sf (gethash "widget_channel_id" table))
                         :system-id (%maybe-sf (gethash "sytem_channel_id" table))
                         :joined-at "joined_at"
                         :large "large"
                         :available (not (gethash "unavailable" table))
                         :member-cnt "member_count"
                         :members (mapvec (curry #'parse-with-gid #'from-json 'member)
                                          (gethash "members" table))
                         :channels (mapvec (curry #'parse-with-gid #'cache 'channel)
                                           (gethash "channels" table))
                         :presences (mapvec (curry #'from-json 'presence)
                                            (gethash "presences" table)))))

(defun %unavailable-from-json (table)
  (make-instance 'guild
                 :id (parse-snowflake (gethash "id" table))
                 :available (not (gethash "unavailable" table))))

(defmethod update ((table hash-table) (g guild))
  (cache-update (id g) :guild table))

(defmethod update ((table hash-table) (g available-guild))
  (from-table-update (table data)
                     ("id" (id g) (parse-snowflake data))
                     ("name" (name g) data)
                     ("icon" (icon g) data)
                     ("splash" (splash g) data)
                     ("owner_id" (owner-id g) (parse-snowflake data))
                     ("region" (region g) data)
                     ("afk_channel_id" (afk-id g) (parse-snowflake data))
                     ("afk_timeout" (afk-to g) data)
                     ("embed_enabled" (embedp g) data)
                     ("embed_channel_id" (embed-id g) (parse-snowflake data))
                     ("verification_level" (verify-level g) data)
                     ("default_message_notification" (notify-level g) data)
                     ("explicit_content_filter" (content-filter g) data)
                     ("roles" (roles g) (mapvec (curry #'cache :role) data))
                     ("emojis" (emojis g) (mapvec (curry #'cache :emoji) data))
                     ("features" (features g) (coerce data 'vector))
                     ("mfa_level" (mfa-level g) data)
                     ("application_id" (app-id g) (parse-snowflake data))
                     ("widget_enabled" (widgetp g) data)
                     ("widget_channel_id" (widget-id g) (parse-snowflake data))
                     ("system_channel_id" (system-channel-id g) (parse-snowflake data)))
  g)

(defmethod from-json ((c (eql 'guild)) (table hash-table))
  (if (gethash "unavailable" table)
      (%unavailable-from-json table)
      (%available-from-json table)))
