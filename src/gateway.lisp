(in-package :lispcord.gateway)

(defun gateway-url ()
  (doit  (get-rq "gateway")
	 (jparse it)
	 (aget "url" it)
	 (:! dprint :debug "~&Gateway-url: ~a~%" it)
	 (str-concat it +api-suffix+)))



(defun send-payload (bot &key op data)
  (if (or ()))
  (doit (jmake (alist "op" op "d" data))
	(:! dprint :debug "~&Send payload: ~a~%" it)
	(wsd:send (bot-conn bot) it)))



;; dispatch events to user handler
(defun dispatch-event (bot event payload)
  (let ((handler (gethash event (bot-callbacks bot))))
    (if handler
	(funcall handler payload)
	(dprint :warn "~&Unhandled event ~a~%" event))))


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





(defun presence (&optional game-name (status "online"))
  `(("game" . ,(if game-name
		   `(("name" . ,game-name) ("type" . 0))
		   :null))
    ("status" . status)
    ("since" . :null)
    ("afk" . :false)))


(defun send-identify (bot)
  (dprint :info "~&Send identify for ~a~%" (bot-token bot))
  (send-payload bot
		:op 2
		:data `(("token" . ,(bot-token bot))
			("properties" . (("$os" . ,+os+)
					 ("$browser" . ,+lib+)
					 ("$device" . ,+lib+)))
			("compress" . :false)
			("large_threshold" . 250)
			("shard" . (0 1))
			("presence" . ,(presence)))))

(defun send-resume (bot)
  (dprint :info "~&Resuming connection for session ~a...~%"
	  (bot-session-id bot))
  (send-payload bot
		:op 6
		:data `(("token" . ,(bot-token bot))
			("session_id" . ,(bot-session-id bot))
			("seq" . ,(bot-seq bot)))))


(defun on-ready (bot payload)
  (dprint :info "~&Ready payload received; Session-id: ~a~%"
	  (aget "session_id" payload))
  (setf (bot-session-id bot) (aget "session_id" payload))
  (setf (bot-user bot) (aget "id" (aget "user" payload)))
  ;dispatch event
  (cargo-send >status> :ready payload (bot-user bot)))




(defun send-status-update (bot &optional game-name (status "online"))
  (send-payload bot 3 (presence game-name status)))




(defun send-heartbeat (bot)
  (send-payload bot 1 (bot-seq bot)))

(defun make-heartbeat-thread (bot seconds)
  (dprint :info "~&Initiating heartbeat every ~d seconds~%" seconds)
  (make-thread (lambda ()
		 (loop :until (bot-done bot) :do
		   (send-heartbeat bot)
		   (sleep seconds)))))



(defun on-members-chunk (data origin)
  (with-table (data id "guild_id" members "members")
    (mapf members (m)
      (setf (gethash "guild_id" m) id)
      (cargo-send >guild> :member-add m origin))))



;; opcode 0
(defun on-dispatch (bot msg)
  (let ((event (aget "t" msg))
	(seq (aget "s" msg))
	(data (aget "d" msg))
	(origin (bot-user bot)))
    (setf (bot-seq bot) seq)
    (dprint :info "[Event] ~a~%" event)
    (dprint :debug "[Payload] ~a~%" msg)
    (str-case event
      ;; on handshake
      ("READY"
       (on-ready bot data))                           

      ;; on resume
      ("RESUMED"
       (cargo-send >status> :resumed data origin))

      ;; someone starts typing
      ("TYPING_START"
       (cargo-send >status> :typing-start data origin))

      ("USER_UPDATE" 
       (cargo-send >user> :update data origin))

      ;; channel made known
      ("CHANNEL_CREATE"
       (cargo-send >channel> :create data origin))

      ("CHANNEL_UPDATE"
       (cargo-send >channel> :update data origin))

      ("CHANNEL_DELETE"
       (cargo-send >channel> :delete data origin))

      ("CHANNEL_PINS_UPDATE"
       (cargo-send >channel> :pins-update data origin))

      ;; guild made known
      ("GUILD_CREATE"
       (cargo-send >guild> :create data origin))

      ("GUILD_UPDATE"
       (cargo-send >guild> :update data origin))

      ("GUILD_DELETE"
       (cargo-send >guild> :delete data origin))

      ("GUILD_BAN_ADD"
       (cargo-send >user> :banned data origin))

      ("GUILD_BAN_REMOVE"
       (cargo-send >user> :unbanned data origin))

      ("GUILD_EMOJIS_UPDATE"
       (cargo-send >guild> :emojis-update data origin))

      ("GUILD_INTEGRATIONS_UPDATE"
       (cargo-send >guild> :integrations-update data origin))

      ("GUILD_MEMBER_ADD"
       (cargo-send >guild> :member-add data origin))

      ("GUILD_MEMBER_REMOVE"
       (cargo-send >guild> :member-remove data origin))

      ("GUILD_MEMBER_UPDATE"
       (cargo-send >guild> :member-update data origin))

      ("GUILD_MEMBERS_CHUNK"
       (on-members-chunk data origin))

      ("GUILD_ROLE_CREATE"
       (cargo-send >guild> :role-create data origin))

      ("GUILD_ROLE_DELETE"
       (cargo-send >guild> :role-delete data origin))

      ("GUILD_ROLE_UPDATE"
       (cargo-send >guild> :role-update data origin))

      ;; received new message
      ("MESSAGE_CREATE"
       (cargo-send >message> :create data origin))

      ;; a message is edited
      ("MESSAGE_UPDATE"
       (cargo-send >message> :update data origin))

      ;; a message is deleted
      ("MESSAGE_DELETE"
       (cargo-send >message> :delete data origin))

      ("MESSAGE_DELETE_BULK"
       (cargo-send >message> :delete-bulk data origin))

      ("MESSAGE_REACTION_ADD"
       (cargo-send >message> :reaction-add data origin))

      ("MESSAGE_REACTION_REMOVE"
       (cargo-send >message> :reaction-remove data origin))

      ("MESSAGE_REACTION_REMOVE_ALL"
       (cargo-send >message> :reaction-purge data origin))
      
      ;; someone updates their presence
      ("PRESENCE_UPDATE"
       (cargo-send >user> :presence data origin))

      ;; unrecognised event!
      (:else
       (dprint :warn "unrecognised event! ~a~%" event)))))





;; opcode 10
(defun on-hello (bot msg)
  (let ((heartbeat-interval (aget "heartbeat_interval" (aget "d" msg))))
    (dprint :debug "Heartbeat Inverval: ~a~%" heartbeat-interval)
    (setf (bot-heartbeat-thread bot)
	  (make-heartbeat-thread bot (/ heartbeat-interval 1000.0)))
    (if (bot-session-id bot)
	(send-resume bot)
	(send-identify bot))))





;; receive message from websocket and dispatch to handler
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
	  (lambda (error)
	    (dprint :error "Websocket error: ~a~%" error)))
  
  (wsd:on :close (bot-conn bot)
	  (lambda (&key code reason)
	    (dprint :warn "Websocket closed with code: ~a~%Reason: ~a~%" code reason))))

(defun disconnect (bot)
  (wsd:close-connection (bot-conn bot))
  (setf (bot-done bot) t)
  (setf (bot-seq bot) nil)
  (setf (bot-session-id bot) nil)
  (setf (bot-user user) nil)
  (setf (bot-heartbeat-thread bot) nil))
