(in-package :lispcord.gateway)

(defun gateway-url ()
  (doit (get-rq "gateway")
	(jparse it)
	(aget "url" it)
	(str-concat it api-suffix)))

(defun send-payload (bot op d)
  ;; make json and send
  (let ((payload (jmake (alist "op" op "d" d))))
    (print (format t "send-payload: ~a~%" payload))
    (wsd:send (bot-conn bot) payload)))


;;wasn't this deprecated in the api?
(defun presence (game-name &optional (status "online"))
  (alist "game" (alist "name" game-name
		       "type" 0)
        "status" status
        "since" (get-universal-time)
        "afk" :false))

(defun send-identify (bot)
  (send-payload bot 2
		(alist "token" (bot-token bot)
		       "properties" (alist "$os" (bot-os bot)
					   "$browser" (bot-lib bot)
					   "$device" (bot-lib bot))
		       "compress" :false
		       "large_threshold" 250
		       "shard" '(0 1)
		       "presence" (presence "hello there" "online"))))

(defun send-status-update (bot)
  (send-payload bot 3 (presence "hello!" "online")))

(defun send-heartbeat (bot)
  (send-payload bot 1 (bot-seq bot)))

(defun make-heartbeat-thread (bot seconds)
  (make-thread (lambda ()
		 (loop
		   (send-heartbeat bot)
		   (sleep seconds)))))


(defun on-message (bot msg)
  (format t "[Message] ~a: ~a~%" (aget "username" (aget "author" msg)) (aget "content" msg))
  (if (not (aget "bot" (aget "author" msg))) (reply bot msg "yo  yo yo")))

;; opcode 0
(defun on-dispatch (bot msg)
  (let ((event (aget "t" msg)) (seq (aget "s" msg)) (d (aget "d" msg)))
    (setf (bot-seq bot) seq)
    (format t "[Event] ~a~%" event)
    (format t "[Payload] ~a~%" msg)
    (str-case event
      ;; on handshake
      ("READY" T)
      ;; someone starts typing somewhere
      ("TYPING_START" T)
      ;; existance of a channel is made known
      ("CHANNEL_CREATE" T)
      ;; existance of a guild is made known
      ("GUILD_CREATE" T)
      ;; a new message is received
      ("MESSAGE_CREATE" (on-message bot d))
      ;; a message is edited
      ("MESSAGE_UPDATE" T)
      ;; a message is deleted
      ("MESSAGE_DELETE" T)
      ;; changed from 'error' to avoid crashing on an undocumented event for now
      (:else (warn "Received invalid event! ~a~%" event)))))

;; opcode 10
;; not sure how we should actually be passing this bot arg around still ^^;
(defun on-hello (bot msg)
  (let ((heartbeat-interval (aget "heartbeat_interval" (aget "d" msg))))
    (format t "Heartbeat Inverval: ~a~%" heartbeat-interval)
    ;; how to add a this thread to the bot struct?
    ;(setf (bot-heartbeat-thread bot) (make-heartbeat-thread bot (/ heartbeat-interval 1000.0)))
    (make-heartbeat-thread bot (/ heartbeat-interval 1000.0))
    ;; should be able to know whether to _identify_ anew or _resume_ here?
    (send-identify bot)))

;; receive message from websock and dispatch to handler
(defun on-recv (bot msg)
  (let ((op (aget "op" msg)))
    (case op
      (0  (on-dispatch bot msg))
      (1  (print msg))
      (7  (print msg))
      (9  (print msg))
      (10 (on-hello bot msg))
      (11 (format t "Received Heartbeat ACK~%"))
      (T ;; not sure if this should be an error to the user or not?
       (error "Received invalid opcode! ~a~%" op)))))


(defun connect (bot)
  (setf (bot-conn bot) (wsd:make-client (gateway-url)))
  (wsd:start-connection (bot-conn bot))
  (wsd:on :open (bot-conn bot)
	  (lambda ()
	    (format t "Connected!~%")))
  (wsd:on :message (bot-conn bot)
	  (lambda (message)
	    (on-recv bot (jparse message))))
  (wsd:on :error (bot-conn bot)
	  (lambda (error) (warn "Websocket error: ~a~%" error)))
  (wsd:on :close (bot-conn bot)
	  (lambda (&key code reason)
	    (format t "Websocket closed with code: ~a~%Reason: ~a~%" code reason))))

