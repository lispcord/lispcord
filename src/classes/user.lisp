(in-package :lispcord.classes)


(defclass user ()
  ((id            :initarg :id
		  :type snowflake
		  :accessor id)
   (username      :initarg :username
		  :type string
		  :accessor username)
   (discriminator :initarg :discrim
		  :type string
		  :accessor discrim)
   (avatar        :initarg :avatar
		  :type string
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
   (guild-id      :initarg :gid
		  :type (or null snowflake)
		  :accessor guild-id)))

(defmethod %to-json ((u user))
  (with-object
    (write-key-value "id" (id u))
    (write-key-value "username" (username u))
    (write-key-value "discriminator" (discrim u))
    (write-key-value "avatar" (avatar u))
    (write-key-value "bot" (botp u))
    (write-key-value "mfa" (mfa-p u))
    (write-key-value "verified" (verifiedp u))
    (write-key-value "email" (emailp u))))

(defmethod from-json ((c (eql :user)) (table hash-table))
  (instance-from-table (table 'user)
    :id "id"
    :username "username"
    :discrim "discriminator"
    :avatar "avatar"
    :bot "bot"
    :mfa "mfa"
    :verified "verified"
    :email "email"
    :gid "guild_id"))



