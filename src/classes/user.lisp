(in-package :lispcord.classes)

(defclass game ()
  ((name :initarg :name
         :type string
         :accessor name)
   (type :initarg :type
         :type (cl:member 0 1 2 4)
         :accessor type)
   (url  :initarg :url
         :type (or null string)
         :accessor url)))

(defun make-game (game-name &optional (type 0) (url nil))
  (make-instance 'game :name game-name :type type :url url))

(define-converters (game)
  name type url)

(defclass user ()
  ((id            :initarg :id
                  :type snowflake
                  :accessor id)
   (username      :initarg :username
                  :type string
                  :accessor name)
   (discriminator :initarg :discrim
                  :type string
                  :accessor discrim)
   (avatar        :initarg :avatar
                  :type (or null string)
                  :accessor avatar)
   (bot           :initarg :bot
                  :type t
                  :accessor botp)
   (mfa           :initarg :mfa
                  :type t
                  :accessor mfa-p)
   (verified      :initarg :verified
                  :type t
                  :accessor verifiedp)
   (email         :initarg :email
                  :type t
                  :accessor emailp)
   (status        :type (or null string)
                  :accessor status)
   (game          :type (or null game)
                  :accessor game)))

(define-converters (user)
  (id 'parse-sowflake)
  username discriminator avatar bot mfa verified email)

(defclass webhook ()
  ((id         :initarg :id
         :type snowflake
         :accessor id)
   (guild-id   :initarg :g-id
         :type (or null snowflake)
         :accessor guild-id)
   (channel-id :initarg :c-id
         :type snowflake)
   (user       :initarg :user
         :type (or null user)
         :accessor user)
   (name       :initarg :name
         :type string
         :accessor name)
   (avatar     :initarg :avatar
         :type string
         :accessor avatar)
   (token      :initarg :token
         :type string
         :accessor token)))

(define-converters (webhook)
  (id 'parse-snowflake)
  (guild-id 'parse-snowflake)
  (channel-id 'parse-snowflake)
  (user (caching-reader 'user))
  avatar token)

(defclass ready ()
  ((version     :initarg :v
    :type fixnum
    :accessor version)
   (user        :initarg :me
          :type user
          :accessor user)
   (channels :initarg :channels
    :type array
    :accessor channels)
   (guilds      :initarg :guilds
    :type array
    :accessor guilds)
   (session-id  :initarg :session
    :type string
    :accessor session-id)))

(define-converters (ready)
  v
  (me (caching-reader 'user))
  (channels (cachine-vector-reader 'channel))
  (guilds (caching-vector-reader 'guild))
  session-id)

(defmethod from-json ((c (eql 'ready)) (table hash-table))
  (instance-from-table (table 'ready)
    :v "v"
    :me (cache 'user (gethash "user" table))
    :channels (mapvec (curry #'cache :channel)
          (gethash "private_channels" table))
    :guilds (mapvec (curry #'cache :guild)
        (gethash "guilds" table))
    :session "session_id"))
