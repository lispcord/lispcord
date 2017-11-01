(in-package :lispcord.core)


(defstruct (bot (:constructor primitive-make-bot))
  (token "" :type string :read-only t)
  (version "0.0.1" :type string)
  (seq 0 :type fixnum)
  (session-id nil :type (or null string))
  conn
  (callbacks (make-hash-table) :type hash-table)
  heartbeat-thread)



(defparameter bot-url "N/A")
(defun bot-url (url)
  (setf bot-url url))


(defun user-agent (bot)
  (str-concat "DiscordBot (" bot-url ", " (bot-version bot) ")"))

(defun headers (bot)
  (list (cons "Authorization" (str-concat "Bot " (bot-token bot)))
        (cons "User-Agent" (user-agent bot))))


(defun get-rq (endpoint &optional bot
	       &aux (url (str-concat +base-url+ endpoint)))
  (dprint :debug "~&HTTP-Get-Request to: ~a~%" url)
  (dex:get url :headers (if bot (headers bot))))

(defun post-rq (endpoint &optional bot content
		&aux (url (str-concat +base-url+ endpoint)))
  (dprint :debug "~&HTTP-Post-Request to: ~a~%~@[   content: ~a~%"
	  url content)
  (dex:post url :headers (if bot (headers bot))
	    :content content))
