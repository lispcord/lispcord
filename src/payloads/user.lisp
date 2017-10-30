;;;; The User Object

(in-package :lispcord.payloads)

(defclass user ()
  ((id            :initarg :id
		  :reader   id
		  :type     string)
   (username      :initarg :username
		  :reader   username
		  :type     string)
   (discriminator :initarg :discrim
		  :reader   discrim
		  :type     string)
   (avatar        :initarg :avatar
		  :reader   avatar
		  :type     string)
   (bot           :initarg :bot
		  :reader   bot?       ;could be nil!
		  :type     (or null t))
   (mfa-enabled   :initarg :mfa
		  :reader   mfa?       ;could be nil!
		  :type     (or null t))
   (verified      :initarg :verified
		  :reader   verified?  ;could be nil!
		  :type     (or null t))
   (email         :initarg :email
		  :reader   email?     ;could be nil!
		  :type     (or null string)))
  (:documentation "Enduser facing USER object"))
