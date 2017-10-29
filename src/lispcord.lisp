(in-package :lispcord)

;;;; This is where all the high level abstractions for the user space
;;;; will go!

(defun make-bot (token &key (version "0.0.1"))
  (unless token (error "No token specified!"))
  (primitive-make-bot :token token
		      :version version))


(defun reply (bot msg content)
  (send bot (aget "channel_id" msg) content))



