(in-package :lispcord.core)


(defstruct (bot (:constructor primitive-make-bot))
  (token "" :type string :read-only t)
  (version "0.0.1" :type string)
  (seq 0 :type fixnum)
  (session-id nil :type (or null string))
  (ratelimit (cons +gw-rate-limit+ (get-universal-time))
	     :type (cons fixnum integer))
  (rl-game (cons +gw-rate-limit-game-status+ (get-universal-time))
	   :type (cons fixnum integer))
  (rl-conn (cons +gw-rate-limit-connection+ (get-universal-time))
	   :type (cons fixnum integer))
  conn
  (callbacks (make-hash-table) :type hash-table)
  heartbeat-thread)

(defvar *bucket* nil)

(defmacro event-rate-limit (bot body)
  `(let ((rl (bot-ratelimit ,bot)))
     (cond ((> 1 (car rl))
	    ,@body)

	   ((time-passed (cdr rl) :minute)
	    (setf (car (bot-ratelimit ,bot)) (1- +gw-rate-limit+)
		  (cdr (bot-ratelimit ,bot)) (get-universal-time))
	    ,@body)

	   (T (dprint :warn "We are being ratelimited!~%")
	      (setf *bucket* (append *bucket* (list (lambda () ,@body))))))))

(defmacro with-rate-limit ((bot type) &body body)
  (case type
    (:http body) ; implement the http-ratelimits here

    (:event `(event-rate-limit ,bot ,body))
    ))


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
