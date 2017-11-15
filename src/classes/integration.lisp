(in-package :lispcord.classes.integration)

(defclass account ()
  ((id   :initarg :id   :type snowflake)
   (name :initarg :name :type string)))

(defmethod from-json ((c (eql :account)) (table hash-table))
  (instance-from-table (table 'account)
    :id "id"
    :name "name"))

(defmethod %to-json ((a account))
  (with-object
    (write-key-value "id" (!! a id))
    (write-key-value "name" (!! a name))))

(defclass integration ()
  ((id               :initarg :id         :type snowflake)
   (name             :initarg :name       :type string)
   (type             :initarg :type       :type string)
   (enabled          :initarg :enabled    :type t)
   (syncing          :initarg :syncing    :type t)
   (role-id          :initarg :role-id    :type snowflake)
   (expire-behavior  :initarg :e-behavior :type fixnum)
   (expire-grace     :initarg :e-grace    :type fixnum)
   (user             :initarg :user       :type user)
   (account          :initarg :account    :type account)
   (synced-at        :initarg :synced-at  :type string)))

(defmethod from-json ((c (eql :integration)) (table hash-table))
  (instance-from-table (table 'integration)
    :id "id"
    :name "name"
    :type "type"
    :enabled "enabled"
    :syning "syncing"
    :role-id "role_id"
    :e-behaviour "expire_behaviour"
    :e-grace "expire_grace_period"
    :user (from-json :user (gethash "user" table))
    :account (from-json :account (gethash "account" table))
    :synced-at "synced-at"))
