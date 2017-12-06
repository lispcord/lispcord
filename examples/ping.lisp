(defpackage :ping.bot (:use :cl :lispcord))
(in-package :ping.bot)

;;make a new bot from a user-given input
(print "Enter a valid token: ")
(defparameter *client* (make-bot (read-line)))

;;connect to the gateway
(connect *client*)

;;set up a handler waiting for "message_create" events
(pmap >message-create> (lambda (msg) (if (equal (lc:content msg) "ping")
					 (reply msg "pong!"))))


