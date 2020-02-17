(in-package :lispcord.classes)

(defclass* account ()
  ((id   :type string)
   (name :type string)))

(define-converters (account)
  id name)

(defclass integration (integration-object)
  ((id        :type snowflake)
   (name      :type string)
   (type      :type string)
   (enabled   :type t)
   (syncing   :type t)
   (role-id   :type snowflake)
   (expire-behavior :type fixnum)
   (expire-grace-period :type fixnum)
   (user      :type user)
   (account   :type account)
   (synced-at :type string)))

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
