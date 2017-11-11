(in-package :lispcord.classes.user)

(defclass user ()
  ((id            :initarg :id
		  :type snowflake)
   (username      :initarg :username
		  :type string)
   (discriminator :initarg :discrim
		  :type string)
   (avatar        :initarg :avatar
		  :type string)
   (bot           :initarg :bot
		  :type t)
   (mfa           :initarg :mfa
		  :type t)
   (verified      :initarg :verified
		  :type t)
   (email         :initarg :email
		  :type t)))

(defmethod %to-json ((u user))
  (with-object
    (write-key-value "id" (!! u id))
    (write-key-value "username" (!! u username))
    (write-key-value "discriminator" (!! u discriminator))
    (write-key-value "avatar" (!! u avatar))
    (write-key-value "bot" (!! u bot))
    (write-key-value "mfa" (!! u mfa))
    (write-key-value "verified" (!! u verified))
    (write-key-value "email" (!! u email))))

(defmethod from-json ((c (eql :user)) (table hash-table))
  (instance-from-table (table 'user)
    :id "id"
    :username "username"
    :discrim "discriminator"
    :avatar "avatar"
    :bot "bot"
    :mfa "mfa"
    :verified "verified"
    :email "email"))
