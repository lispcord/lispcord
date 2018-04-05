(in-package :lispcord.classes)


(defclass account ()
  ((id   :initarg :id
   :type string
   :accessor id)
   (name :initarg :name
   :type string
   :accessor name)))

(defmethod from-json ((c (eql :account)) (table hash-table))
  (instance-from-table (table 'account)
    :id "id"
    :name "name"))

(defmethod %to-json ((a account))
  (with-object
    (write-key-value "id" (id a))
    (write-key-value "name" (name a))))

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
   (expire-behavior  :initarg :e-behavior
         :type fixnum
         :accessor expire-behaviour)
   (expire-grace     :initarg :e-grace
         :type fixnum
         :accessor expire-grace)
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

(defmethod from-json ((c (eql :integration)) (table hash-table))
  (instance-from-table (table 'integration)
    :id (parse-snowflake (gethash "id" table))
    :name "name"
    :type "type"
    :enabled "enabled"
    :syning "syncing"
    :role-id (%maybe-sf (gethash "role_id" table))
    :e-behaviour "expire_behaviour"
    :e-grace "expire_grace_period"
    :user (cache :user (gethash "user" table))
    :account (from-json :account (gethash "account" table))
    :synced-at "synced-at"))
