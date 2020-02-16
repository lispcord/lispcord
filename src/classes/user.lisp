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


(defmethod from-json ((c (eql 'game)) (table hash-table))
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

(defmethod %to-json ((u user))
  (with-object
    (write-key-value "id" (id u))
    (write-key-value "username" (name u))
    (write-key-value "discriminator" (discrim u))
    (write-key-value "avatar" (avatar u))
    (write-key-value "bot" (botp u))
    (write-key-value "mfa" (mfa-p u))
    (write-key-value "verified" (verifiedp u))
    (write-key-value "email" (emailp u))))

(defmethod from-json ((c (eql 'user)) (table hash-table))
  (instance-from-table (table 'user)
                       :id (parse-snowflake (gethash "id" table))
                       :username "username"
                       :discrim "discriminator"
                       :avatar "avatar"
                       :bot "bot"
                       :mfa "mfa"
                       :verified "verified"
                       :email "email"))





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

(defmethod from-json ((c (eql 'webhook)) (table hash-table))
  (instance-from-table (table 'webhook)
    :id (parse-snowflake (gethash "id" table))
    :g-id (parse-snowflake (gethash "guild_id" table))
    :c-id (parse-snowflake (gethash "channel_id" table))
    :user (cache 'user (gethash "user" table))
    :avatar "avatar"
    :token "token"))



(defclass ready ()
  ((version     :initarg :v
    :type fixnum
    :accessor version)
   (user        :initarg :me
          :type user
          :accessor user)
   (dm-channels :initarg :channels
    :type array
    :accessor channels)
   (guilds      :initarg :guilds
    :type array
    :accessor guilds)
   (session-id  :initarg :session
    :type string
    :accessor session-id)))

(defmethod from-json ((c (eql 'ready)) (table hash-table))
  (instance-from-table (table 'ready)
    :v "v"
    :me (cache 'user (gethash "user" table))
    :channels (mapvec (curry #'cache :channel)
          (gethash "private_channels" table))
    :guilds (mapvec (curry #'cache :guild)
        (gethash "guilds" table))
    :session "session_id"))
