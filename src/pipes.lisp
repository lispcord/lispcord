(in-package :lispcord.pipes)

(declaim (inline make-event-table))
(defun make-event-table ()
  "Returns a fresh empty hash-table"
  (make-hash-table :test #'eq :size 45))

(defun add-event-handler (event-key event-handler &optional (bot *client*))
  (declare (type keyword event-key)
     (type function event-handler)
     (type bot bot))
  (setf (gethash event-key (bot-event-handlers bot)) event-handler))

(defun dispatch-event (event-key args &optional (bot *client*))
  (apply (gethash event-key (bot-event-handlers bot)
      (lambda (&rest _)
        (declare (ignore _))
        nil))
   args))


