(in-package :lispcord.core)

(defparameter *client* nil
  "This is an empty dummy var to allow for implicits.
It may be set by make-bot!")

(defstruct (bot (:constructor primitive-make-bot))
  (token "" :type string :read-only t)
  (user nil :type (or null lc:user))
  (version "0.0.1" :type string)
  (seq 0 :type fixnum)
  (session-id nil :type (or null string))
  (afk-since nil :type (or null fixnum))
  (event-handlers (make-hash-table) :type hash-table)
  conn
  heartbeat-thread)


;;; Set up the various event pipes
;;; By (my) convention, they should be named ">'name'>"
#|
(defvar >status-ready> (make-pipe))
(defvar >status-close> (make-pipe))
(defvar >status-resumed> (make-pipe))

(defvar >user-create> (make-pipe))
(defvar >user-delete> (make-pipe))
(defvar >user-update> (make-pipe))

(defvar >emoji-update> (make-pipe))
(defvar >emoji-update> (make-pipe))
(defvar >emoji-update> (make-pipe))

(defvar >member-add> (make-pipe))
(defvar >member-update> (make-pipe))
(defvar >member-remove> (make-pipe))
(defvar >member-ban> (make-pipe))
(defvar >member-unban> (make-pipe))

(defvar >role-create> (make-pipe))
(defvar >role-update> (make-pipe))
(defvar >role-delete> (make-pipe))

(defvar >channel-create> (make-pipe))
(defvar >channel-delete> (make-pipe))
(defvar >channel-update> (make-pipe))

(defvar >pin-update> (make-pipe))

(defvar >presence-update> (make-pipe))

(defvar >typing-start> (make-pipe))

(defvar >integrations-update> (make-pipe))

(defvar >guild-create> (make-pipe))
(defvar >guild-update> (make-pipe))
(defvar >guild-delete> (make-pipe))

(defvar >message-create> (make-pipe))
(defvar >message-update> (make-pipe))
(defvar >message-delete> (make-pipe))
(defvar >message-purge> (make-pipe))

(defvar >reaction-add> (make-pipe))
(defvar >reaction-remove> (make-pipe))
(defvar >reaction-purge> (make-pipe))
|#


(defparameter *bot-url* "N/A")
(defun bot-url (url)
  (setf *bot-url* url))


(defun user-agent (bot)
  (str-concat "DiscordBot (" *bot-url* ", " (bot-version bot) ")"))

(defun headers (bot)
  (list (cons "Authorization" (str-concat "Bot " (bot-token bot)))))




(defun discord-req (endpoint
		    &key bot content
		      (content-type "application/json")
		      (type :get)
		      parameters
		    &aux
		      (url (str-concat +base-url+ endpoint))
		      (final (rl-buffer endpoint)))
  (dprint :debug "~&HTTP-~a-Request to: ~a~%~@[  content: ~a~%~]"
	  type url content)
  (multiple-value-bind (body status headers uri
			     stream closedp reason)
      (drakma:http-request
       url
       :method type
       :parameters parameters
       :content-type content-type
       :content content
       :user-agent (if bot (user-agent bot) :drakma)
       :additional-headers (if bot (headers bot))
       :external-format-in :utf8
       :external-format-out :utf8)
    (declare (ignore uri stream closedp reason))
    (rl-parse final headers)
    (case status
      (400 (cerror "ignore" "HTTP: BAD REQUEST"))
      (401 (cerror "ignore" "HTTP: UNAUTHORIZED"))
      (403 (cerror "ignore" "HTTP: FORBIDDEN"))
      (405 (cerror "ignore" "HTTP: BAD METHOD"))
      (408 (cerror "ignore" "HTTP: TIMEOUT"))
      (429 (cerror "ignore" "HTTP: RATELIMIT"))
      (520 (cerror "ignore" "HTTP: UNKNOWN")))
    (values (cond ((= status 204) t)
		  ((= status 404) nil)
		  (t (jparse (babel:octets-to-string body))))
	    status)))

(defun get-rq (endpoint &optional bot)
  (discord-req endpoint :bot bot :type :get))

(defun post-rq (endpoint &optional bot content)
  (discord-req endpoint :bot bot :content content :type :post))


