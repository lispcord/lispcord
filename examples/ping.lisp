(ql:quickload :lispcord)
(use-package :lispcord)

;;make a new bot from a user-given input
(print "Enter a valid token: ")
(defparameter *token* (read-line))

(defvar *client* (make-bot *token*))

;;connect to the gateway
(connect *client*)

;;set up a handler waiting for "message_create" events
(watch-with-case (>message> msg)
		 (:create (format t "Got message :D~%~a~%"
				  (lc:content msg))))


