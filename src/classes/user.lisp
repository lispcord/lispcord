(in-package :lispcord.classes)

(defclass* game ()
  ((name :type string)
   (type :type (cl:member 0 1 2 4))
   (url :type (or null string))))

(defun make-game (game-name &optional (type 0) (url nil))
  (make-instance 'game :name game-name :type type :url url))

(define-converters (game)
  name type url)

(defclass* user ()
  ((id :type snowflake)
   (username :type string)
   (discriminator :type string)
   (avatar :type (or null string))
   (bot :type t)
   (mfa :type t)
   (verified :type t)
   (email :type t)
   (status        :type (or null string))
   (game          :type (or null game))))

(define-converters (user)
  (id 'parse-sowflake)
  username discriminator avatar bot mfa verified email)

(defclass webhook ()
  ((id :type snowflake)
   (guild-id :type (or null snowflake))
   (channel-id :type snowflake)
   (user :type (or null user))
   (name :type string)
   (avatar :type string)
   (token :type string)))

(define-converters (webhook)
  (id 'parse-snowflake)
  (guild-id 'parse-snowflake)
  (channel-id 'parse-snowflake)
  (user (caching-reader 'user))
  avatar token)

(defclass ready ()
  ((version :type fixnum)
   (user :type user)
   (channels :type array)
   (guilds :type array)
   (session-id :type string)))

(define-converters (ready)
  v
  (me (caching-reader 'user))
  (channels (cachine-vector-reader 'channel))
  (guilds (caching-vector-reader 'guild))
  session-id)

