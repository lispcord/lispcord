;;;; The Ready Event Payload

(in-package :lispcord.payloads)

(defclass ready ()
  ((user             :initarg :user
	             :reader   user
	             :type     user)
   (private-channels :initarg :pms
		     :reader   pms
		     :type     (vector dm-channel))
   (guilds           :initarg :guilds
	             :reader   :guilds
	             :type     (vector guilds))))
