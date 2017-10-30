;;;; The User Object

(defclass user ()
  ((id            :init-arg :id
		  :reader   id
		  :type     string)
   (username      :init-arg :username
		  :reader   username
		  :type     string)
   (discriminator :init-arg :discrim
		  :reader   discrim
		  :type     string)
   (avatar        :init-arg :avatar
		  :reader   avatar
		  :type     string)
   (bot           :init-arg :bot
		  :reader   bot?       ;could be nil!
		  :type     (or null t))
   (mfa-enabled   :init-arg :mfa
		  :reader   mfa?       ;could be nil!
		  :type     (or null t))
   (verified      :init-arg :verified
		  :reader   verified?  ;could be nil!
		  :type     (or null t))
   (email         :init-arg :email
		  :reader   email?     ;could be nil!
		  :type     (or null string)))
  (:documentation "Enduser facing USER object"))
