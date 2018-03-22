(in-package :example-bot)

(defbot *client* "TOKEN")

(defun message-create (msg)
  (cond ((lc:botp (lc:author msg)) nil)
	((not (commandp msg)) nil)
	(t (dispatch-commands msg))))

;;set up a handler waiting for "message_create" events
(add-event-handler :on-message-create #'message-create)


(defun main ()
  (connect *client*))
