(in-package :lispcord.gateway)

(defun gateway-url ()
  (doit  (get-rq "gateway")
	 (jparse it)
	 (aget "url" it)
	 (:! dprint :debug "~&Gateway-url: ~a~%" it)
	 (str-concat it api-suffix)))

(defun send-payload (bot op d)
  (doit (jmake (alist "op" op "d" d))
	(:! dprint :debug "~&Send payload: ~a~%" it)
	(wsd:send (bot-conn bot) it)))



;; dispatch events to user handler
(defun dispatch-event (bot event payload)
  (let ((handler (gethash event (bot-callbacks bot))))
    (if handler
	(funcall handler payload)
	(dprint :warn "~&Unhandled event ~a~%" event))))





(defun send-identify (bot)
  (dprint :info "~&Send identify for ~a~%" (bot-token bot))
  (send-payload bot 2
		(alist "token" (bot-token bot)
		       "properties" (alist "$os" (bot-os bot)
					   "$browser" (bot-lib bot)
					   "$device" (bot-lib bot))
		       "compress" :false
		       "large_threshold" 250
		       "shard" '(0 1)
		       "presence" (presence))))

(defun send-resume (bot)
  (dprint :info "~&Resuming connection for session ~a...~%"
	  (bot-session-id bot))
  (send-payload bot 6
		(alist "token" (bot-token bot)
		       "session_id" (bot-session-id bot)
		       "seq" (bot-seq bot))))


(defun on-ready (bot payload)
  (dprint :info "~&Ready payload received; Session-id: ~a~%"
	  (aget "session_id" payload))
  (setf (bot-session-id bot) (aget "session_id" payload))
  ;dispatch event
  (dispatch-event bot :ready payload))





(defun presence (&optional game-name (status "online"))
  (alist "game" (if game-name
		  (alist "name" game-name
		         "type" 0)
		  :null)
        "status" status
	;; apparently this can be null if not idle?
;        "since" (get-universal-time)
        "since" :null
        "afk" :false))


(defun send-status-update (bot &optional game-name (status "online"))
  (send-payload bot 3 (presence game-name status)))




(defun send-heartbeat (bot)
  (send-payload bot 1 (bot-seq bot)))

(defun make-heartbeat-thread (bot seconds)
  (dprint :info "~&Initiating heartbeat every ~d seconds~%" seconds)
  (make-thread (lambda ()
		 (loop
		   (send-heartbeat bot)
		   (sleep seconds)))))






(defun on-message (bot msg)
  (declare (ignorable bot))
  (dprint :info "[Message] ~a: ~a~%"
	  (aget "username" (aget "author" msg))
	  (aget "content" msg))
  ;;implement the user-facing event handling
  (dispatch-event bot :message msg))




;; opcode 0
(defun on-dispatch (bot msg)
  (let ((event (aget "t" msg)) (seq (aget "s" msg)) (d (aget "d" msg)))
    (setf (bot-seq bot) seq)
    (dprint :info "[Event] ~a~%" event)
    (dprint :debug "[Payload] ~a~%" msg)
    (str-case event
      ("READY" (on-ready bot d))                           ; on handshake
      ("RESUME" (dprint :info "Connection resumed!"))      ; on resume
      ("TYPING_START" (dispatch-event bot :typing d))      ; someone starts typing
      ("CHANNEL_CREATE" (dispatch-event bot :channel d))   ; channel made known
      ("GUILD_CREATE" (dispatch-event bot :guild d))       ; guild made known
      ("MESSAGE_CREATE" (on-message bot d))                ; received new message
      ("MESSAGE_UPDATE" (dispatch-event bot :edit d))      ; a message is edited
      ("MESSAGE_DELETE" (dispatch-event bot :delete d))    ; a message is deleted
      ("PRESENCE_UPDATE" (dispatch-event bot :presence d)) ; someone updates their presence
      (:else (dprint :warn "Received invalid event! ~a~%" event)))))





;; opcode 10
(defun on-hello (bot msg)
  (let ((heartbeat-interval (aget "heartbeat_interval" (aget "d" msg))))
    (dprint :debug "Heartbeat Inverval: ~a~%" heartbeat-interval)
    (setf (bot-heartbeat-thread bot)
	  (make-heartbeat-thread bot (/ heartbeat-interval 1000.0)))
    (if (bot-session-id bot)
	(send-resume bot)
	(send-identify bot))))





;; receive message from websock and dispatch to handler
(defun on-recv (bot msg)
  (let ((op (aget "op" msg)))
    (case op
      (0  (on-dispatch bot msg)) ;Dispatch Event
      (1  (send-heartbeat bot))  ;Requests Heartbeat
      (7  (print msg))           ;Requests Reconnect
      (9  (print msg))           ;Invalid Sessions Event
      (10 (on-hello bot msg))    ;Hello Event
      (11 (dprint :debug "Received Heartbeat ACK~%"))
      (T ;; not sure if this should be an error to the user or not?
       (dprint :error "Received invalid opcode! ~a~%" op)))))






(defun connect (bot)
  (setf (bot-conn bot) (wsd:make-client (gateway-url)))
  (wsd:start-connection (bot-conn bot))
  (wsd:on :open (bot-conn bot)
	  (lambda ()
	    (dprint :info "Connected!~%")))
  (wsd:on :message (bot-conn bot)
	  (lambda (message)
	    (on-recv bot (jparse message))))
  (wsd:on :error (bot-conn bot)
	  (lambda (error) (warn "Websocket error: ~a~%" error)))
  (wsd:on :close (bot-conn bot)
	  (lambda (&key code reason)
	    (dprint :info "Websocket closed with code: ~a~%Reason: ~a~%" code reason))))

