(in-package :lispcord.core)

(defvar *client* nil
  "This is an empty dummy var to allow for implicits.
It may be set by make-bot!")

(defstruct (bot (:constructor primitive-make-bot)
		:conc-name)
  (token "" :type string :read-only t)
  (user nil :type (or null lc:user))
  (version "0.0.1" :type string)
  (seq 0 :type fixnum)
  (session-id nil :type (or null string))
  (afk-since nil :type (or null fixnum))
  conn
  (done nil :type (or null t))
  heartbeat-thread)


;;; Set up the various event pipes
;;; By (my) convention, they should be named ">'name'>"

(defvar >status> (make-pipe)
  "The generic event pipe")

(defvar >user> (make-pipe)
  "Dispatches user specific events")

(defvar >channel> (make-pipe)
  "Dispatches channel specific events")

(defvar >guild> (make-pipe)
  "Dispatches guild specific events")

(defvar >message> (make-pipe)
  "Dispatches message specific events")





(defparameter bot-url "N/A")
(defun bot-url (url)
  (setf bot-url url))


(defun user-agent (bot)
  (str-concat "DiscordBot (" bot-url ", " (version bot) ")"))

(defun headers (bot)
  (list (cons "Authorization" (str-concat "Bot " (token bot)))))




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
       :external-format-out :utf8
       :decode-content t)
    (declare (ignore uri stream closedp reason))
    (rl-parse final headers)
    (values (if (= status 204)
		t
		(jparse (babel:octets-to-string body))) status)))

(defun get-rq (endpoint &optional bot)
  (discord-req endpoint :bot bot :type :get))

(defun post-rq (endpoint &optional bot content)
  (discord-req endpoint :bot bot :content content :type :post))


