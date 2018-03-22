(in-package :lispcord)

;;;; This is where all the high level abstractions for the user space
;;;; will go!

(defun make-bot (token &key (version "0.0.1"))
  (unless token (error "Token required!"))
  (let ((b (primitive-make-bot :token token :version version)))
    (setf *client* b)
    b))



;;; prefixes

(defvar *cmd-prefix-table* (make-hash-table))

(defun make-prefix (character &rest guilds)
  (declare (type character character))
  (setf (gethash character *cmd-prefix-table*)
	(if guilds guilds :global)))

(defun commandp (msg &optional (bot *client*))
  (declare (type lc:message msg))
  (when (find (bot-user bot) (lc:mentions msg) :test #'eq)
    (return-from commandp t))
  (let ((cmd? (gethash (char (lc:content msg) 0)
		       *cmd-prefix-table*)))
    (cond ((not cmd?) nil)
	  ((eq :global cmd?) t)
	  ((consp cmd?) (member (lc:guild msg) cmd?
				:test #'eq))
	  (t (warn "the object interned for prefix ~a is not a list or the keyword \":global\"" (char (lc:content msg) 0))))))

(defun sanitize-content (content)
  (declare (type string content))
  (substitute #\EXTRATERRESTRIAL_ALIEN #\@ content))


(defmacro defbot (symbol token
		  &key
		    prefix
		    (version "0.0.1"))
  `(progn
     (defparameter ,symbol (make-bot ,token
				     :version ,version))
     (when ,prefix (make-prefix ,prefix))
     ,symbol))

;;; useful functions



(defun me (&optional (bot *client*))
  (bot-user bot))

(defun reply (msg content &optional (bot *client*))
  (create content (from-id (lc:channel-id msg) :channel) bot))

(defun remove-substring (string msg)
  (declare (type string string msg))
  (let ((from (search string msg))
	(to (length string)))
    (when from
      (str-concat (subseq msg 0 from) (subseq msg to)))))

(defun remove-mention (user msg)
  (declare (type lc:user user)
	   (type string msg))
  (or (remove-substring (str-concat "<@" (to-string (lc:id user)) ">")
			msg)
      (remove-substring (str-concat "<@!" (to-string (lc:id user)) ">")
			msg)))
