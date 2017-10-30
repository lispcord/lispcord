;;;; The emoji object

(in-package :lispcord.payloads)

(defclass emoji ()
  ((id             :initarg :id
                   :reader   id
                   :type     string)
   (name           :initarg :name
	           :reader   name
	           :type     string)
   (roles          :initarg :roles
	           :reader   roles
	           :type     (vector string))
   (user           :initarg :user
	           :reader    user? ; could be nil!
	           :type      (or null user))
   (require-colons :initarg :colons
		   :reader colonsp
		   :type (or null t))
   (managed        :initarg :managed
	           :reader   managedp
	           :type     (or null t))))
