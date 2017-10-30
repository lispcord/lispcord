;;;; The Roles object

(in-package :lispcord.payloads)

(defclass role ()
  ((id          :initarg :id
		:reader  id
		:type    string)
   (name        :initarg :name
		:reader  name
		:type    string)
   (color       :initarg :color
		:reader  color
		:type    fixnum)
   (hoist       :initarg :hoist
		:reader  hoistp
		:type    (or null t))
   (rposition    :initarg :rposition
		:reader  rposition
		:type    fixnum)
   (permissions :initarg :perms
		:reader  perms
		:type    fixnum)
   (managed     :initarg :managed
		:reader  managedp
		:type    (or null t))
   (mentionable :initarg :mentionable
		:reader  mentionablep
		:type    (or null t)))
  (:documentation "The basic roles object"))
