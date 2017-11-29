(in-package :lispcord.http)

(defvar *client* nil
  "This is an empty dummy var to allow for implicits.
It may be set by make-bot!")

; send a message!
(defun send (bot channel-id content)
  (post-rq (str-concat "channels/" channel-id "/messages")
	   bot
	   `(("content" . ,content))))





(defgeneric from-id (snowflake from &optional bot)
  (:documentation "Retrieves the given object via ID, either from the cache or through a REST call.
FROM can be one of :CHANNEL, :GUILD, :USER, :EMOJI, :ROLE or a 
discord-place like a channel, a guild and so on"))

(defgeneric create (content destination &optional bot)
  (:documentation "Sends a POST-request to create the given
object."))

(defgeneric edit (content destination &optional bot)
  (:documentation "Sends a PUT or PATCH request to modify
the given object"))

(defgeneric erase (object &optional bot)
  (:documentation "Sends a DELETE request to remove the
given object"))

(defmethod erase ((n null) &optional (bot *client*))
  (declare (ignore n bot))
  nil)
