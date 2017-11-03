(in-package :lispcord)

;;;; This is where all the high level abstractions for the user space
;;;; will go!

(defun make-bot (token &key (version "0.0.1"))
  (unless token (error "No token specified!"))
  (primitive-make-bot :token token
		      :version version))


;; we can re-export #'connect, but i thought about making a defbot
;; tbh, which would define various things á la defclass or defstruct

;;; useful functions

(defun reply (bot msg content)
  (send bot (aget "channel_id" msg) content))



;;; event handler macro
;; handle :message events like this:
; bot = bot instance
; :message is the event type
; payload is the parameter name for the payload in the body
; (with-handler (payload bot :message) ...)
(defmacro with-handler ((var bot event) &body body)
  `(setf (gethash ,event (bot-callbacks ,bot))
	 (lambda (,var) ,@body)))
