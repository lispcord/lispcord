(in-package :lispcord.classes)

(defclass* game ()
  ((name :type string)
   (type :type (cl:member 0 1 2 4))
   (url :type (or null string))))

(export-pub make-game)
(defun make-game (game-name &optional (type 0) (url nil))
  (make-instance 'game :name game-name :type type :url url))

(define-converters (%to-json from-json) game
  name type url)

(defclass* user ()
  ((id :type snowflake)
   (username :type string)
   (discriminator :type string)
   (avatar :type (or null string))
   (bot :type boolean :accessor bot-p)
   (system :type boolean :accessor system-p)
   (mfa-enabled :type boolean :accessor mfa-enabled-p)
   (locale :type (or null string))
   (verified :type boolean :accessor verified-p)
   (email :type (or null string))
   (flags :type (or null fixnum))
   (premium-type :type (or null fixnum))
   (status :type (or null string))
   (game :type (or null activity))))

(defmethod from-json ((c (eql :user)) table)
  (from-json 'user table))

(define-converters (%to-json from-json) user
  (id 'parse-snowflake)
  username discriminator avatar
  (bot nil (defaulting-writer :false))
  (system nil (defaulting-writer :false))
  (mfa-enabled nil (defaulting-writer :false))
  (locale)
  (verified nil (defaulting-writer :false))
  email flags premium-type
  ;; Internal Lispcord fields (pieces of last Presence event for this user)
  (status :ignore :ignore)
  (game :ignore :ignore))

(defclass* webhook ()
  ((id :type snowflake)
   (type :type fixnum)
   (guild-id :type (or null snowflake))
   (channel-id :type snowflake)
   (user :type (or null user))
   (name :type string)
   (avatar :type string)
   (token :type string)))

(define-converters (%to-json from-json) webhook
  (id 'parse-snowflake)
  (type)
  (guild-id '%maybe-sf)
  (channel-id 'parse-snowflake)
  (user (caching-reader 'user))
  avatar token)

(defclass* ready ()
  ((v :type fixnum)
   (user :type user)
   (private-channels :type (vector dm-channel))
   (guilds :type (vector unavailable-guild))
   (session-id :type string)
   ;; Not handled by Lispcord
   ;;(shard :type (array fixnum 2))
   ;; Undocumented
   ;;(user-settings)
   ;;(relationships)
   ))

(define-converters (%to-json from-json) ready
  v
  (user (caching-reader 'user))
  (private-channels (caching-vector-reader :channel))
  (guilds (caching-vector-reader :guild))
  session-id
  ;; Not handled by Lispcord
  ;;(shard)
  ;; Undocumented
  ;;(user-settings)
  ;;(relationships)
  )

(defclass* activity ()
  ((name :type string)
   (type :type fixnum)
   (url :type (or null string))
   (created-at :type fixnum)
   (timestamps :type (or null timestamps))
   (application-id :type (or null snowflake))
   (details :type (or null string))
   (state :type (or null string))
   (emoji :type (or null emoji))
   (party :type (or null party))
   (assets :type (or null assets))
   (secrets :type (or null secrets))
   (instance :type boolean)
   (flags :type (or null fixnum))))

(define-converters (%to-json from-json) activity
  name type url created-at
  (timestamps (subtable-reader 'activity-timestamps))
  (application-id '%maybe-sf)
  (details)
  (state)
  (emoji (subtable-reader 'activity-emoji))
  (party (subtable-reader 'activity-party))
  (assets (subtable-reader 'activity-assets))
  (secrets (subtable-reader 'activity-secrets))
  (instance)
  (flags))

(defclass* activity-timestamps ()
  ((start :type (or null fixnum))
   (end :type (or null fixnum))))

(define-converters (%to-json from-json) activity-timestamps
  start end)

(defclass* activity-emoji ()
  ((name :type string)
   (id :type (or null snowflake))
   (animated :type boolean)))

(define-converters (%to-json from-json) activity-emoji
  name
  (id '%maybe-sf)
  animated)

(defclass* activity-party ()
  ((id :type (or null string))
   (size :type (or null (array fixnum)))))

(define-converters (%to-json from-json) activity-party
  id
  (size (vector-reader 'identity)))

(defclass* activity-assets ()
  ((large-image :type (or null string))
   (large-text :type (or null string))
   (small-image :type (or null string))
   (small-text :type (or null string))))

(define-converters (%to-json from-json) activity-assets
    large-image large-text small-image small-text)

(defclass* activity-secrets ()
  ((join :type (or null string))
   (spectate :type (or null string))
   (match :type (or null string))))

(define-converters (%to-json from-json) activity-secrets
    join spectate match)


