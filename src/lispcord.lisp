(in-package :lispcord)

;;;; This is where all the high level abstractions for the user space
;;;; will go!

(defun make-bot (token &key (version "0.0.1"))
  (unless token (error "No token specified!"))
  (primitive-make-bot :token token
		      :version version))


;; we can re-export #'connect, but i thought about making a defbot
;; tbh, which would define various things á la defclass or defstruct
;; -----
;; should this be here? so lispcord.gateway doesnt have to be exposed?
(defun connect-bot (bot)
  (connect bot))



;;; useful functions

(defun reply (bot msg content)
  (send bot (aget "channel_id" msg) content))



;;; event handler macro
;; honestly not sure the best way to do this xD
;; how should the payload parameter fit into this?
;; also, were we supposed to have separate macros for each event?
;; or one macro that just takes the event to register?
;; and why is it saying 'payload is unbound' ? 

;; handle :message events like this:
; bot = bot instance
; :message is the event type
; payload is the parameter name for the payload in the body
; (with-bot-message (bot :message payload) ...)
(defmacro with-bot-message (vars &body body)
  (destructuring-bind (bot event var) vars
    `(setf (gethash ,event (bot-callbacks ,bot))
    	   (lambda (,var) ,@body))))
