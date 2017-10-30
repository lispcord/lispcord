;;;; The Ready Event Payload


(defclass ready ()
  ((v                :init-arg :version
                     :reader   version
                     :type     fixnum)
   (user             :init-arg :user
	             :reader   user
	             :type     user)
   (private-channels :init-arg :pms
		     :reader   pms
		     :type     (vector dm-channel))
   (guilds           :init-arg :guilds
	             :reader   :guilds
	             :type     (vector guilds))))
