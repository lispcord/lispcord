(in-package :lispcord.gateway)

(defun gateway-url ()
  (doit  (get-rq "gateway")
	 (jparse it)
	 (aget "url" it)
	 (:! dprint :debug "~&Gateway-url: ~a~%" it)
	 (str-concat it +api-suffix+)))



(defun send-payload (bot &key op data)
  (doit (jmake `(("op" . ,op) ("d" . ,data)))
	(:! dprint :debug "~&Send payload: ~a~%" it)
	(wsd:send (bot-conn bot) it)))

(defun make-status (bot status game afk
		    &aux (since (if afk (if (bot-afk-since bot)
					    (bot-afk-since bot)
					    (setf (bot-afk-since bot)
						  (unix-epoch))))))
  `(("since" . ,since)
    ("game" . ,game)
    ("afk" . ,afk)
    ("status" . ,(string-downcase (string status)))))

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
			("presence" . ,(make-status bot
						    :online
						    nil
						    nil)))))

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
  (setf (bot-user bot) (aget "user" payload))
  ;dispatch event
  (cargo-send >status> :ready payload (!! (bot-user bot) id)))




(defun send-status-update (bot &optional game (status :online))
  (send-payload bot
		:op 3
		:data (make-status bot status game nil)))




(defun send-heartbeat (bot)
  (send-payload bot
		:op 1
		:data (bot-seq bot)))

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


(defun on-emoji-update (data origin)
  (with-table (data emojis "emojis")
    (sethash "emojis" data
	  (map 'vector (curry #'from-json :emoji) emojis))
    (cargo-send >guild> :emojis-update data origin)))


(defun on-member-remove (data origin)
  (let ((user (from-json :user (gethash "user" data))))
    (setf (slot-value user 'guild-id) (gethash "guild_id" data))
    (cargo-send >guild> :member-remove user origin)))

(defun on-member-update (data origin)
  (let ((member (from-json :g-member data)))
    (cargo-send >guild> :member-update member origin)))

(defun on-role-* (data origin kind)
  (let ((role (from-json :role (gethash "role" data))))
    (setf (slot-value role 'guild-id) (gethash "guild_id" data))
    (cargo-send >guild> kind origin)))



;; opcode 0
(defun on-dispatch (bot msg)
  (let ((event (aget "t" msg))
	(seq (aget "s" msg))
	(data (aget "d" msg))
	(origin (!! (bot-user bot) id)))
    (setf (bot-seq bot) seq)
    (dprint :info "[Event] ~a~%" event)
    (dprint :debug "[Payload] ~a~%" msg)
    (str-case event
      ;; on handshake
      ("READY"
       (on-ready bot data))                           

      ;; on resume
      ("RESUMED"
       (cargo-send >status> :resumed nil origin))

      ;; someone starts typing
      ("TYPING_START"
       (cargo-send >status> :typing-start data origin))

      ("USER_UPDATE" 
       (cargo-send >user> :update (from-json :user data) origin))

      ;; channel made known
      ("CHANNEL_CREATE"
       (cargo-send >channel> :create (from-json :channel data) origin))

      ("CHANNEL_UPDATE"
       (cargo-send >channel> :update (from-json :channel data) origin))

      ("CHANNEL_DELETE"
       (cargo-send >channel> :delete (from-json :channel data) origin))

      ("CHANNEL_PINS_UPDATE"
       (cargo-send >channel> :pins-update data origin))

      ;; guild made known
      ("GUILD_CREATE"
       (cargo-send >guild> :create (from-json :guild data) origin))

      ("GUILD_UPDATE"
       (cargo-send >guild> :update (from-json :guild data) origin))

      ("GUILD_DELETE"
       (cargo-send >guild> :delete (from-json :guild data) origin))

      ("GUILD_BAN_ADD"
       (cargo-send >user> :banned (from-json :user data) origin))

      ("GUILD_BAN_REMOVE"
       (cargo-send >user> :unbanned (from-json :user data) origin))

      ("GUILD_EMOJIS_UPDATE"
       (on-emoji-update data origin))

      ("GUILD_INTEGRATIONS_UPDATE"
       (cargo-send >guild>
		   :integrations-update
		   (gethash "guild_id" data)
		   origin))

      ("GUILD_MEMBER_ADD"
       (cargo-send >guild> :member-add (from-json :g-member data) origin))

      ("GUILD_MEMBER_REMOVE"
       (on-guild-member-remove data origin))

      ("GUILD_MEMBER_UPDATE"
       (on-member-update data origin))

      ("GUILD_MEMBERS_CHUNK"
       (on-members-chunk data origin))

      ("GUILD_ROLE_CREATE"
       (on-role-* data origin :role-create))

      ("GUILD_ROLE_DELETE"
       (cargo-send >guild> :role-delete data origin))

      ("GUILD_ROLE_UPDATE"
       (on-role-* data origin :role-update))

      ;; received new message
      ("MESSAGE_CREATE"
       (cargo-send >message> :create (from-json :message data) origin))

      ;; a message is edited
      ("MESSAGE_UPDATE"
       (cargo-send >message> :update (from-json :message data) origin))

      ;; a message is deleted
      ("MESSAGE_DELETE"
       (cargo-send >message> :delete data origin))

      ("MESSAGE_DELETE_BULK"
       (sethash "ids" data (coerce (gethash "ids" data) 'vector))
       (cargo-send >message> :delete-bulk data origin))

      ("MESSAGE_REACTION_ADD"
       (sethash "emoji" data (from-json :emoji (gethash "emoji" data)))
       (cargo-send >message> :reaction-add data origin))

      ("MESSAGE_REACTION_REMOVE"
       (sethash "emoji" data (from-json :emoji (gethash "emoji" data)))
       (cargo-send >message> :reaction-remove data origin))

      ("MESSAGE_REACTION_REMOVE_ALL"
       (cargo-send >message> :reaction-purge data origin))
      
      ;; someone updates their presence
      ("PRESENCE_UPDATE"
       (cargo-send >guild> :presence (from-json :presence data) origin))

      ;; unrecognised event!
      (:else
       (dprint :warn "unrecognised event! ~a~%" event)
       (cargo-send >status> :unrecognised-event nil origin)))))


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
	    (cargo-send >status> :close (list code reason) (bot-user bot))
	    (dprint :warn "Websocket closed with code: ~a~%Reason: ~a~%" code reason))))

(defun disconnect (bot)
  (wsd:close-connection (bot-conn bot))
  (setf (bot-done bot) t)
  (setf (bot-seq bot) 0)
  (setf (bot-session-id bot) nil)
  (setf (bot-user bot) nil)
  (setf (bot-heartbeat-thread bot) nil))
