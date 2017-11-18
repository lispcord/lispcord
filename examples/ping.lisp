(use-package :lispcord)

;;make a new bot from a user-given input
(defvar *client* (make-bot (read-line)))

;;connect to the gateway
(connect *client*)

;;set up a handler waiting for "message_create" events
(watch-with-case (>message> msg)
		 (:create (format t "Got message :D~%~a~%"
				  (!! msg content))))


