(in-package :lispcord.core)

(defstruct (bot (:constructor primitive-make-bot))
  (token "" :type string :read-only t)
  (os (software-type) :type string)
  (lib "lispcord" :type string)
  (version "0.0.1" :type string)
  (seq 0 :type fixnum)
  (session-id nil :type (or null string))
  heartbeat-thread
  conn)


(defparameter bot-url "N/A")
(defun bot-url (url)
  (setf bot-url url))

(defparameter base-url "https://discordapp.com/api/v6/")
(defparameter api-suffix "?v=6&encoding=json")
(defun api-version (version)
  (setf base-url (str-concat "https://discordapp.com/api/v" version "/")
	api-suffix (str-concat "?v=" version "&encoding=json")))



(defun user-agent (bot)
  (str-concat "DiscordBot (" bot-url ", " (bot-version bot) ")"))

(defun headers (bot)
  (list (cons "Authorization" (str-concat "Bot " (bot-token bot)))
        (cons "User-Agent" (user-agent bot))))


(defun get-rq (endpoint &optional bot &aux (url (str-concat base-url endpoint)))
  (dex:get url :headers (if bot (headers bot)))
  (dprint :debug "~&HTTP-Get-Request to: ~a~%" url))

(defun post-rq (endpoint &optional bot content
			   &aux (url (str-concat base-url endpoint)))
  (dex:post url :headers (if bot (headers bot))
	    :content content)
  (dprint :debug "~&HTTP-Post-Request to: ~a~%~@[   content: ~a~%" url content))
