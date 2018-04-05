(defpackage :ping.bot
  (:use :cl :lispcord))
(in-package :ping.bot)

;;make a new bot from a user-given input
(print "Enter a valid token: ")
(defbot *ping-bot* (read-line))

;;connect to the gateway
(connect *ping-bot*)

;;specify the behaviour of the bot for "on-message-create" events:
(defun message-create (msg)
  (cond ((lc:botp (lc:author msg)) nil) ; short out if the author is a bot
  ((not (commandp msg)) nil) ; short out if the message is not a command
  ;; If the command was invoked via an @mention, we want to get rid of
  ;; that as well as any surrounding whitespace
  ;; #'ME simply returns the user-instance of the current bot
  (t (let ((cmd (string-trim " " (remove-mention (me) (lc:content msg)))))
       (cond ((string= cmd "ping!")
        (let ((now (get-internal-real-time))
        (reply (reply msg "pong!"))
        (then (get-internal-real-time)))
          (edit (format nil "Message took: ~a ms" (- then now))
          reply)))
       ((string= cmd "bye!")  (disconnect *ping-bot*)))))))

;;set up a handler waiting for "message_create" events
(add-event-handler :on-message-create #'message-create)


