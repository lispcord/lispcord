(defpackage :ping.bot
  (:use :cl :lispcord))
(in-package :ping.bot)

;;make a new bot from a user-given input
(print "Enter a valid token: ")
(defbot *ping-bot* (read-line))

;;connect to the gateway
(connect *ping-bot*)

;;set up a handler waiting for "message_create" events
(defpipe >handler> :for msg :from >message-create>
	 :do (if (equal (lc:content msg) "ping")
		 (reply msg "pong")))


