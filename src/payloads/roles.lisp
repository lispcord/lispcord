;;;; The Roles object

(defclass role ()
  (id          :init-arg :id
               :reader   id
               :type     string)
  (name        :init-arg :name
	       :reader   name
	       :type     string)
  (color       :init-arg :color
	       :reader   color
	       :type     fixnum)
  (hoist       :init-arg :hoist
	       :reader   hoistp
	       :type     (or null t))
  (position    :init-arg :position
	       :reader   position
	       :type     fixnum)
  (permissions :init-arg :perms
	       :reader   perms
	       :type     fixnum)
  (managed     :init-arg :managed
	       :reader   managedp
	       :type     (or null t))
  (mentionable :init-arg :mentionable
	       :reader   mentionablep
	       :type     (or null t))
  (:documentation "The basic roles object"))
