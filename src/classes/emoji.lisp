(in-package :lispcord.classes)

(defclass partial-emoji ()
  ((name :initarg :name :accessor name)
   (image :initarg :image :accessor image)
   (roles :initarg :roles :accessor roles)))

(defun make-emoji (name image &optional roles)
  (make-instance 'new-emoji
                 :name name
                 :image image
                 :roles roles))

(define-converters (partial-emoji)
  name image
  (roles 'identity (defaulting-writer :null)))

(defclass emoji ()
  ((id       :initarg :id
             :type (or null snowflake)
             :accessor id)
   (name     :initarg :name
             :type (or null string)
             :accessor name)
   (roles    :initarg :roles
             :type (vector snowflake)
             :accessor roles)
   (user     :initarg :user
             :type (or null user)
             :accessor user)
   (require-colons :initarg :require-colons
             :type boolean
             :accessor colonsp)
   (managed  :initarg :managed
             :type boolean
             :accessor managedp)
   (animated :initarg :animated
             :type boolean
             :accessor animatedp)
   (guild-id :initarg :gid
             :type (or null snowflake)
             :accessor guild-id)))

(define-converters (emoji)
  (id    'parse-snowflake)
  (name)
  (roles (vector-reader 'parse-snowflake))
  (user  (caching-reader 'user))
  (require-colons)
  (managed)
  (animated))
