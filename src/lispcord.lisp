(in-package :lispcord)

;;;; This is where all the high level abstractions for the user space
;;;; will go!

(defun make-bot (token &key (version "0.0.1"))
  (unless token (error "Token required!"))
  (let ((b (%make-bot :token token :version version)))
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
  (unless (string= "" (lc:content msg))
    (let ((cmd? (gethash (char (lc:content msg) 0)
                         *cmd-prefix-table*)))
      (cond ((not cmd?) nil)
            ((eq :global cmd?) t)
            ((consp cmd?) (member (lc:guild msg) cmd?
                                  :test #'eq))
            (t (warn "the object interned for prefix ~a is not a list or the keyword \":global\"" (char (lc:content msg) 0)))))))

(defun sanitize-content (content)
  (declare (type string content))
  #+sbcl (substitute #\EXTRATERRESTRIAL_ALIEN #\@ content)
  #-sbcl content)


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
                        msg)
      msg))

(defun mention (mentionable)
  "Convert a mentionable object to the format used in Discord messages"
  (declare (type (or lc:user lc:channel lc:role lc:emoji)
                 mentionable))
  (format nil
          (typecase mentionable
            (lc:user "<@~a>")
            (lc:channel "<#~a>")
            (lc:role "<@&~a>")
            (lc:emoji (str-concat (if (lc:animatedp mentionable) "<a:" "<:")
                                  (lc:name mentionable)
                                  ":~a>")))
          (lc:id mentionable)))

(defun demention (string &aux (len (1- (length string))))
  "Parse a mentionable from the string and convert it to a mentionable object"
  (labels ((emoji-snowflake (n)
             (let ((name-begin (position #\: string :start n :end len)))
               (if name-begin
                   (snowflake (1+ name-begin))
                   (return-from demention))))
           (snowflake (n)
             (or (parse-snowflake (subseq string n len))
                 (return-from demention))))
    (when (> (length string) 3)
      (let ((one (char string 0))
            (two (char string 1))
            (thr (char string 2))
            (lst (char string len)))
        (when (and (char= one #\<)
                   (char= lst #\>))
          (case two
            (#\#
             (getcache-id (snowflake 2) :channel))
            (#\@
             (case thr
               (#\! (getcache-id (snowflake 3) :user))
               (#\& (getcache-id (snowflake 3) :role))
               (t (getcache-id (snowflake 2) :user))))
            (#\:
             (getcache-id (emoji-snowflake 2) :emoji))
            (#\a
             (when (char= thr #\:)
               (getcache-id (emoji-snowflake 3) :emoji)))))))))

