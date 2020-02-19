(in-package :lispcord.classes)

(defclass* partial-emoji ()
  ((name)
   (image)
   (roles)))

(export-pub make-emoji)
(defun make-emoji (name image &optional roles)
  (make-instance 'new-emoji
                 :name name
                 :image image
                 :roles roles))

(define-converters (partial-emoji)
  name image
  (roles 'identity (defaulting-writer :null)))

(defclass* emoji ()
  ((id       :type (or null snowflake))
   (name     :type (or null string))
   (roles    :type (vector snowflake))
   (user     :type (or null user))
   (require-colons :type boolean :accessor require-colons-p)
   (managed  :type boolean :accessor managed-p)
   (animated :type boolean :accessor animated-p)
   (guild-id :type (or null snowflake))))

(define-converters (emoji)
  (id    'parse-snowflake)
  (name)
  (roles (vector-reader 'parse-snowflake))
  (user  (caching-reader 'user))
  (require-colons)
  (managed)
  (animated)
  (guild-id :ignore :ignore))
