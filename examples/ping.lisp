(defpackage :ping.bot (:use :cl :lispcord))
(in-package :ping.bot)

;;make a new bot from a user-given input
(print "Enter a valid token: ")
(defvar *client* (make-bot (read-line)))

;;connect to the gateway
(connect *client*)

;;set up a handler waiting for "message_create" events
(pmap >message>
      (lambda (e)
	(cargocase e
	  ((:create msg) (reply msg "pong")))))


