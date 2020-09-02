(defpackage :ping.bot
  (:use :cl :lispcord))
(in-package :ping.bot)

(defvar *ping-bot*)

;;specify the behaviour of the bot for "on-message-create" events:
(defun message-create (msg)
  (when (and (not (botp (lc:author msg))) (commandp msg))
    ;; If the command was invoked via an @mention, we want to get rid of
    ;; that as well as any surrounding whitespace
    ;; #'ME simply returns the user-instance of the current bot
    (let ((cmd (string-trim " " (remove-mention (me) (lc:content msg)))))
      (cond ((string= cmd "ping!")
             (let ((now (get-internal-real-time))
                   (reply (reply msg "pong!"))
                   (then (get-internal-real-time)))
               (edit (format nil "Message took: ~a ms" (- then now))
                     reply)))
            ((string= cmd "bye!")  (disconnect *ping-bot*))))))


(defun run (&optional token)
  (unless token
    (print "Enter a valid token: ")
    (setf token (read-line)))
  (setf *ping-bot* (make-bot token))
  ;; set up a handler waiting for message_create events
  (add-event-handler :on-message-create 'message-create)
  (connect *ping-bot*))

