(in-package :lispcord.classes)

(defclass account ()
  ((id   :initarg :id
         :type string
         :accessor id)
   (name :initarg :name
         :type string
         :accessor name)))

(define-converters (account)
  id name)

(defclass integration (integration-object)
  ((id               :initarg :id
                     :type snowflake
                     :accessor id)
   (name             :initarg :name
                     :type string
                     :accessor name)
   (type             :initarg :type
                     :type string
                     :accessor type)
   (enabled          :initarg :enabled
                     :type t
                     :accessor enabledp)
   (syncing          :initarg :syncing
                     :type t
                     :accessor syncingp)
   (role-id          :initarg :role-id
                     :type snowflake
                     :accessor role-id)
   (expire-behavior  :initarg :expire-behavior
                     :type fixnum
                     :accessor expire-behaviour)
   (expire-grace-period :initarg :expire-grace-period
                     :type fixnum
                     :accessor expire-grace-period)
   (user             :initarg :user
                     :type user
                     :accessor user)
   (account          :initarg :account
                     :type account
                     :accessor account)
   (synced-at        :initarg :synced-at
                     :type string
                     :accessor synced-at)))

(defmethod role ((i integration))
  (getcache-id (role-id i) :role))

(define-converters (integration)
  (id 'parse-snowflake)
  name type enabled syncing
  (role-id '%maybe-sf)
  (expire-behaviour)
  (expire-grace-period)
  (user (caching-reader 'user))
  (account (subtable-reader 'account))
  (synced-at))
