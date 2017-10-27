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

(defun send-payload (bot op d)
  ;; make json and send
  (let ((payload (jonathan:to-json (list :|op| op :|d| d))))
    (print (format t "send-payload: ~a~%" payload))
    (wsd:send (bot-connection bot) payload)))

(defun presence (game-name status)
  (list :|game| (list :|name| "presence"
                      :|type| 0)
        :|status| "online"
        :|since| (get-universal-time)
        :|afk| :false))

(defun send-identify (bot)
  (send-payload bot 2 (list :|token| (bot-token bot)
                            :|properties| (list :|$os| os-string
                                                :|$browser| lib-string
                                                :|$device| lib-string)
                            :|compress| :false
                            :|large_threshold| 250
                            :|shard| '(1 10)
                            :|presence| (presence "hello there" "online"))))

(defun send-status-update (bot)
  (send-payload bot 3 (presence "hello!" "online")))

(defun send-heartbeat (bot)
  (send-payload bot 1 last-seq))

(defun heartbeat-timer (bot interval)
  ;; check to make sure bot is connected still first
  (send-heartbeat bot)
  (schedule-timer (make-timer (lambda () (heartbeat-timer bot heartbeat)) interval)))

;; opcode 0
;; this last seq var should be associated with the bot struct, not global
(defvar last-seq nil)
(defun on-recv-dispatch (bot msg)
  (let ((event (getf msg :|t|)) (seq (getf msg :|s|)))
    (setf last-seq seq)
    (format t "Event: ~a~%" event)
    (format t "Payload: ~a~%" msg)
    ;; is there a case form for strings?
    (cond
      ;; unsure if this is necessary
      ((string= event "READY") (send-status-update bot))
      (T (error "Received invalid event! ~a~%" event)))))

;; opcode 10
;; not sure how we should actually be passing his bot arg around still ^^;
(defun on-recv-hello (bot msg)
  (let ((heartbeat (getf (getf msg :|d|) :|heartbeat_interval|)))
    (format t "Heartbeat Inverval: ~a~%" heartbeat)
    ;; setup heartbeat interval here
    (heartbeat-timer bot (/ heartbeat 1000))
    ;; should be able to know whether to _identify_ anew or _resume_ here?
    (send-identify bot)))

;; receive message from websock and dispatch to handler
(defun on-recv (bot msg)
  (let ((op (getf msg :|op|)))
    (case op
      (0  (on-recv-dispatch bot msg))
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
      (11 (format t "Received Heartbeat ACK~%"))
      (T ;; not sure if this should be an error to the user or not?
       (error "Received invalid opcode! ~a~%" op)))))


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
