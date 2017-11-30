(in-package :lispcord)

;;;; This is where all the high level abstractions for the user space
;;;; will go!

(defun make-bot (token &key (version "0.0.1"))
  (unless token (error "Token required!"))
  (setf *client*
	(primitive-make-bot :token token
			    :version version)))

(defun me (&optional (bot *client*))
  (user bot))

;; we can re-export #'connect, but i thought about making a defbot
;; tbh, which would define various things á la defclass or defstruct

;;; useful functions

(defun reply (msg content &optional (bot *client*))
  (create content (lc:author msg) bot))


