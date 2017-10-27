(in-package :lispcord)

;; make dynamic?
(defparameter os-string "linux")
(defparameter lib-string "lispcord")

(defparameter bot-url "N/A")
(defun bot-url (url)
  (setf bot-url url))

(defparameter base-url "https://discordapp.com/api/v6/")
(defun api-version (version)
  (str-concat "https://discordapp.com/api/" version "/"))

(defparameter api-suffix "/?v=6&encoding=json")

(defstruct (bot (:constructor primitive-bot-make))
  (token "" :type string :read-only t)
  (version "0.0.1" :type string)
  connection)

(defun make-bot (token &key (version "0.0.1"))
  (unless token (error "No token specified!"))
  (primitive-bot-make :token token
		      :version version))

(defun user-agent (bot)
  (str-concat "DiscordBot (" bot-url ", " (bot-version bot) ")"))

(defun headers (bot &optional (length 0))
  (list (cons "Authorization" (str-concat "Bot " (bot-token bot)))
        (cons "User-Agent" (user-agent bot))
        (cons "Content-length" (format nil "~a" length))))

;; is 'get' reserved?
(defun get-rq (endpoint)
  (dex:get (str-concat base-url "/" endpoint)))

;; i added -rq to make the name match get-rq for now
(defun post-rq (bot endpoint)
  (dex:post (str-concat base-url "/" endpoint)
	    :headers (headers bot)))

(defun gateway ()
  (get-rq "gateway"))

(defun fetch-gateway-url ()
  (cadr (jonathan:parse (gateway))))

;; should this be combined with the above?
(defun gateway-url ()
  (str-concat (fetch-gateway-url) api-suffix))

;; get unix timestamp (millis)
(defun get-millis ()
  0)

(defun send-payload (bot op d)
  (wsd:send (bot-connection bot) (jonathan:to-json (list :op op
							 :d d))))

;; how to put 'false' in the json?
(defun send-identify (bot)
  (let ((identify (list :token (bot-token bot)
			                  :properties (list :$os os-string
					                    :$browser lib-string
						            :$device lib-string)
				          :compress :false
				          :large_threshold 250
				          :shard '(1 10)
				          ;; this should probably be separated out?
				          :presence (list :game (list :name "presence"
						   	              :type 0)
						          :status "online"
						          :since (get-millis)
						          :afk :false))))
    (format t "identify: ~a" identify)
    (send-payload bot 10 identify)))

;; opcode 10
;; not sure how we should actually be passing his bot arg around still ^^;
(defun on-recv-hello (bot msg)
  (let ((heartbeat (getf (getf msg :|d|) :|heartbeat_interval|)))
    (print bot)
    (print (bot-token bot))
    (format t "Heartbeat: ~a" heartbeat)
    ;; setup heartbeat interval here
    ;; should be able to know whether to _identify_ anew or _resume_ here?
    (send-identify bot)))

;; receive message from websock and dispatch to handler
(defun on-recv (bot msg)
  (let ((op (getf msg :|op|)))
    (case op
      (0  (print msg))
      (1  (print msg))
      (2  (print msg))
      (3  (print msg))
      (4  (print msg))
      (5  (print msg))
      (6  (print msg))
      (7  (print msg))
      (8  (print msg))
      (9  (print msg))
      (10 (on-recv-hello bot msg))
      (11 (print msg))
      (T ;; not sure if this should be an error to the user or not?
       (error "Received invalid opcode! ~a" op)))))


(defun connect (bot)
  ;; not sure how these args work, so i'm passing 'bot' from this defun for now to on-recv
  (defvar b bot);
  (setf (bot-connection bot) (wsd:make-client (gateway-url)))
  (wsd:start-connection (bot-connection bot))
  (wsd:on :open (bot-connection bot)
	  (lambda ()
	    (format t "Connected!~%")))
  (wsd:on :message (bot-connection bot)
	  (lambda (message)
	    (on-recv b (jonathan:parse message))))
  (wsd:on :error (bot-connection bot)
	  (lambda (error) (warn "Websocket error: ~a~%" error)))
  (wsd:on :close (bot-connection bot)
	  (lambda (&key code reason)
	    (format t "Websocket closed with code: ~a~%Reason: ~a~%" code reason))))
