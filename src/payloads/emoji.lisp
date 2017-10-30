;;;; The emoji object

(defclass emoji ()
  ((id             :init-arg :id
                   :reader   id
                   :type     string)
   (name           :init-arg :name
	           :reader   name
	           :type     string)
   (roles          :init-arg :roles
	           :reader   roles
	           :type     (vector string))
   (user           :init-arg :user
	           :reader    user? ; could be nil!
	           :type      (or null user))
   (require-colons :init-arg :colons
		   :reader colonsp
		   :type (or null t))
   (managed        :init-arg :managed
	           :reader   managedp
	           :type     (or null t))))
