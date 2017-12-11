(in-package :lispcord.gateway)

(defvar *gateway-url* nil)

(defun refresh-gateway-url ()
  (doit  (get-rq "gateway")
	 (aget "url" it)
	 (:! dprint :debug "~&Gateway-url: ~a~%" it)
	 (str-concat it +api-suffix+)
	 (setf *gateway-url* it)))





(defun send-payload (bot &key op data)
  (doit (jmake `(("op" . ,op) ("d" . ,data)))
	(:! dprint :debug "~&Send payload: ~a~%" it)
	(wsd:send (conn bot) it)))

(defun make-status (bot status game afk)
  (let ((since (when afk (if (afk-since bot)
			     (afk-since bot)
			     (setf (afk-since bot) (since-unix-epoch))))))
    `(("since" . ,since)
      ("game" . ,game)
      ("afk" . ,afk)
      ("status" . ,status))))

(defun send-identify (bot)
  (dprint :info "~&Send identify for ~a~%" (token bot))
  (send-payload bot
		:op 2
		:data `(("token" . ,(str-concat "Bot " (token bot)))
			("properties" . (("$os" . ,+os+)
					 ("$browser" . ,+lib+)
					 ("$device" . ,+lib+)))
			("compress" . :false)
			("large_threshold" . 250)
			("shard" . (0 1))
			("presence" . ,(make-status bot
						    "online"
						    nil
						    nil)))))

(defun send-resume (bot)
  (dprint :info "~&Resuming connection for session ~a...~%"
	  (session-id bot))
  (send-payload bot
		:op 6
		:data `(("token" . ,(token bot))
			("session_id" . ,(session-id bot))
			("seq" . ,(seq bot)))))


(defun on-ready (bot payload)
  (dprint :info "~&Ready payload received; Session-id: ~a~%"
	  (aget "session_id" payload))
  (setf (session-id bot) (aget "session_id" payload))
  (setf (user bot) (cache :user (aget "user" payload)))
  ;;dispatch event
  (cargo-send >status-ready> (list (from-json :ready payload)) bot))




(defun send-status-update (bot &optional game (status :online))
  (send-payload bot
		:op 3
		:data (make-status bot status game nil)))




(defun send-heartbeat (bot)
  (send-payload bot
		:op 1
		:data (seq bot)))

(defun make-heartbeat-thread (bot seconds
			      &optional (stream *error-output*))
  (declare (type rational seconds))
  (dprint :info "~&Initiating heartbeat every ~d seconds~%" seconds)
  (make-thread (lambda ()
		 (let ((*error-output* stream))
		   (loop
		      (dprint :debug "Dispatching heartbeat!")
		      (send-heartbeat bot)
		      (sleep seconds))))))






(defun on-emoji-update (data origin)
  (with-table (data emojis "emojis"
		    id "guild_id")
    (let ((g (cache :guild (new-hash-table `("id" ,id)
					   `("emojis" ,emojis)))))
      (cargo-send >emoji-update>
		  (list (lc:emojis g) g)
		  origin))))


(defun on-member-remove (data origin)
  (let* ((user (getcache-id (parse-snowflake
			    (gethash "id" (gethash "user" data)))
			   :user))
	 (g-id (parse-snowflake (gethash "guild_id" data)))
	 (g (getcache-id g-id :guild)))
    (setf (lc:members g)
	  (vecrem (lambda (e) (eq user (lc:user e))) (lc:members g)))
    (cargo-send >member-remove> (list user g)
		origin)))

(defun on-member-add (data origin)
  (let ((member (from-json :g-member data))
	(g (getcache-id (gethash "guild_id" data) :guild)))
    (setf (lc:members g) (vec-extend member (lc:members g)))
    (cargo-send >member-add> (list member g) origin)))

(defun on-members-chunk (data origin)
  (with-table (data id "guild_id" members "members")
    (mapf members (m)
      (setf (gethash "guild_id" m) id)
      (on-member-add m origin))))

(defun on-member-update (data origin)
  (let ((g (getcache-id (gethash "guild_id" data) :guild))
	(member (from-json :g-member data)))
    (nsubstitute-if member
		    (lambda (m) (eq (lc:user m) (lc:user member)))
		    (lc:members g))
    (cargo-send >member-update> (list member g) origin)))

(defun on-role-add (data origin)
  (let ((role (cache :role (gethash "role" data)))
	(g (getcache-id (parse-snowflake (gethash "guild_id" data))
			:guild)))
    (setf (lc:guild-id role) (parse-snowflake (gethash "guild_id" data)))
    (setf (lc:roles g) (vec-extend role (lc:roles g)))
    (cargo-send >role-create> (list role g) origin)))

(defun on-role-update (data origin)
  (let ((role (cache :role (gethash "role" data)))
	(g (getcache-id (gethash "guild_id" data) :guild)))
    (cargo-send >role-update> (list role g) origin)))

(defun on-role-delete (data origin)
  (with-table (data guild-id "guild_id" role-id "role_id")
    (let* ((g-id (parse-snowflake guild-id))
	   (r-id (parse-snowflake role-id))
	   (g (getcache-id g-id :guild)))
      (decache-id r-id :role)
      (setf (lc:roles g)
	    (vecrem (lambda (e)
		      (funcall optimal-id-compare r-id (lc:id e)))
		    (lc:roles g)))
      (cargo-send >role-delete> (list r-id g-id) origin))))


(defun on-channel-create (data origin)
  (let ((c (cache data :channel)))
    (when (typep c 'lc:guild-channel)
      (let ((g (getcache-id (lc:guild-id c) :guild)))
	(setf (lc:channels g) (vec-extend c (lc:channels g)))))
    (cargo-send >channel-create> (list c) origin)))

(defun on-channel-delete (data origin)
  (let ((c (cache data :channel)))
    (when (typep c 'lc:guild-channel)
      (let ((g (getcache-id (lc:guild-id c) :guild)))
	(setf (lc:channels g)
	      (vecrem (lambda (e) (eq e c)) (lc:channels g)))))
    (decache-id (lc:id c) :channel)
    (cargo-send >channel-delete> (list c) origin)))

(defun on-channel-pin-update (data origin)
  (let ((id (parse-snowflake (gethash "id" data))))
    (cargo-send >pin-update>
		(list (getcache-id id :channel)
		      (gethash "last_pin_timestamp" data))
		origin)))


(defun on-guild-ban (data origin kind)
  (let ((u (cache :user data))
	(g (getcache-id (parse-snowflake (gethash "guild_id" data))
			:guild)))
    (cargo-send kind (list u g) origin)))


(defun on-reaction (data origin kind)
  (let ((e (cache :emoji (gethash "emoji" data)))
	(u (getcache-id (parse-snowflake (gethash "user_id" data))
			:user))
	(c (getcache-id (parse-snowflake (gethash "channel_id" data))
			:channel))
	(mid (parse-snowflake (gethash "message_id" data))))
    (cargo-send kind
		(list e mid u c)
		origin)))


(defun on-presence (data origin)
  (let ((u (getcache-id (parse-snowflake
			 (gethash "id" (gethash "user" data)))
			:user)))
    (when u
      (dprint :debug "User uncached: ~a~%"
	      (gethash "id" (gethash "user" data)))
      (setf (lc:status u) (gethash "status" data))
      (setf (lc:game u) (from-json :game (gethash "game" data))))
    (cargo-send >presence-update>
		(list (from-json :presence data))
		origin)))

(defun on-typing-start (data origin)
  (let ((c (getcache-id (parse-snowflake (gethash "channel_id" data))
			:channel))
	(u (getcache-id (parse-snowflake (gethash "user_id" data))
			:user))
	(ts (gethash "timestamp" data)))
    (cargo-send >typing-start>
		(list u c ts)
		origin)))

;; opcode 0
(defun on-dispatch (bot msg)
  (let ((event (aget "t" msg))
	(seq (aget "s" msg))
	(data (aget "d" msg))
	(origin bot))
    (setf (seq bot) seq)
    (dprint :info "[Event] ~a~%" event)
    (dprint :debug "[Payload] ~a~%" msg)
    (str-case event
      ;; on handshake
      ("READY"
       (on-ready bot data))                           

      ;; on resume
      ("RESUMED"
       (cargo-send >status-resumed> nil origin))

      ;; someone starts typing
      ("TYPING_START"
       (on-typing-start data origin))

      ("USER_UPDATE" 
       (cargo-send >user-update> (list (cache :user data)) origin))

      ;; channel made known
      ("CHANNEL_CREATE"
       (on-channel-create data origin))

      ("CHANNEL_UPDATE"
       (cargo-send >channel-update>
		   (list (cache :channel data))
		   origin))

      ("CHANNEL_DELETE"
       (on-channel-delete data origin))

      ("CHANNEL_PINS_UPDATE"
       (on-channel-pin-update data origin))

      ;; guild made known
      ("GUILD_CREATE"
       (cargo-send >guild-create> (list (cache :guild data)) origin))

      ("GUILD_UPDATE"
       (cargo-send >guild-update> (list (cache :guild data)) origin))

      ("GUILD_DELETE"
       (let ((g (cache :guild data)))
	 (cargo-send >guild-delete> (list g) origin)
	 (decache-id (lc:id g) :guild)))

      ("GUILD_BAN_ADD"
       (on-guild-ban data origin >member-ban>))

      ("GUILD_BAN_REMOVE"
       (on-guild-ban data origin >member-unban>))

      ("GUILD_EMOJIS_UPDATE"
       (on-emoji-update data origin))

      ("GUILD_INTEGRATIONS_UPDATE"
       (cargo-send >integrations-update>
		   (list (getcache-id
			  (parse-snowflake (gethash "guild_id" data))
			  :guild))
		   origin))

      ("GUILD_MEMBER_ADD"
       (on-member-add data origin))

      ("GUILD_MEMBER_REMOVE"
       (on-member-remove data origin))

      ("GUILD_MEMBER_UPDATE"
       (on-member-update data origin))

      ("GUILD_MEMBERS_CHUNK"
       (on-members-chunk data origin))

      ("GUILD_ROLE_CREATE"
       (on-role-add data origin))

      ("GUILD_ROLE_DELETE"
       (on-role-delete data origin))

      ("GUILD_ROLE_UPDATE"
       (on-role-update data origin))

      ;; received new message
      ("MESSAGE_CREATE"
       (cargo-send >message-create>
		   (list (from-json :message data))
		   origin))

      ;; a message is edited // might need special parsing here
      ("MESSAGE_UPDATE"
       (cargo-send >message-update>
		   (list (from-json :message data))
		   origin))

      ;; a message is deleted
      ("MESSAGE_DELETE"
       (cargo-send >message-delete>
		   (list
		    (parse-snowflake (gethash "id" data))
		    (getcache-id
		     (parse-snowflake (gethash "channel_id" data))
		     :channel))
		   origin))

      ("MESSAGE_DELETE_BULK"
       (let ((ids (mapvec #'parse-snowflake (gethash "ids" data)))
	     (c (getcache-id
		 (parse-snowflake (gethash "channel_id" data))
		 :channel)))
	 (cargo-send >message-purge> (list c ids) origin)))

      ("MESSAGE_REACTION_ADD"
       (on-reaction data origin >reaction-add>))

      ("MESSAGE_REACTION_REMOVE"
       (on-reaction data origin >reaction-remove>))

      ("MESSAGE_REACTION_REMOVE_ALL"
       (let ((c (getcache-id
		 (parse-snowflake (gethash "channel_id" data))
		 :channel))
	     (mid (parse-snowflake (gethash "message_id" data))))
	 (cargo-send >reaction-purge>
		     (list c mid)
		     origin)))
      
      ;; someone updates their presence
      ("PRESENCE_UPDATE"
       (on-presence data origin))

      ;; unrecognised event!
      (:else
       (dprint :warn "unrecognised event! ~a~%" event)))))


;; opcode 10
(defun on-hello (bot msg)
  (let ((heartbeat-interval (aget "heartbeat_interval" (aget "d" msg))))
    (declare (type (unsigned-byte 32) heartbeat-interval))
    (dprint :debug "Heartbeat Inverval: ~a~%" heartbeat-interval)
    (setf (heartbeat-thread bot)
	  (make-heartbeat-thread bot (/ heartbeat-interval 1000)))
    (if (session-id bot)
	(send-resume bot)
	(send-identify bot))))




(defun disconnect (bot &optional reason code)
  (let ((out *error-output*))
    (bt:join-thread
     (bt:make-thread
      (lambda ()
	(let ((*error-output* out))
	  (dprint :info "~a disconnecting...~%"
		  (if (user bot) (lc:name (user bot))))
	  (wsd:close-connection (conn bot) reason code)
	  (setf (seq bot) 0)
	  (setf (session-id bot) nil)
	  (bt:destroy-thread (heartbeat-thread bot))))))))

(defun cleanup (bot)
  (dprint :warn "Cleanup loop engaged!~%Bot: ~a" (lc:name (user bot)))
  (disconnect bot)
  (sleep 4)
  (connect bot))

(defun reconnect (bot &optional reason code)
  (dprint :warn "Attempting to reconnect!~%Bot: ~a" (lc:name (user bot)))
  (wsd:close-connection (conn bot) reason code)
  (setf (conn bot) nil)
  (setf (heartbeat-thread bot) nil))


;; receive message from websocket and dispatch to handler
(defun on-recv (bot msg)
  (let ((op (aget "op" msg)))
    (case op
      (0  (on-dispatch bot msg)) ;Dispatch Event
      (1  (send-heartbeat bot))  ;Requests Heartbeat
      (7  (reconnect bot))	 ;Requests Reconnect
      (9  (cleanup bot))         ;Invalid Sessions Event
      (10 (on-hello bot msg))    ;Hello Event
      (11 (dprint :debug "Received Heartbeat ACK~%"))
      (T ;; not sure if this should be an error to the user or not?
       (dprint :error "Received invalid opcode! ~a~%" op)))))


(defun connect (bot)
  (assert (typep bot 'bot))
  (unless *gateway-url* (refresh-gateway-url))
  (setf (conn bot) (wsd:make-client *gateway-url*))
  (wsd:start-connection (conn bot))
  
  (wsd:on :open (conn bot)
	  (lambda ()
	    (dprint :info "Connected!~%")))
  
  (wsd:on :message (conn bot)
	  (lambda (message)
	    (on-recv bot (jparse message))))
  
  (wsd:on :error (conn bot)
	  (lambda (error)
	    (dprint :error "Websocket error: ~a~%" error)
	    (refresh-gateway-url)))
  
  (wsd:on :close (conn bot)
	  (lambda (&key code reason)
	    (let ((reason (typecase reason
			    (null nil)
			    (string reason)
			    (vector (babel:octets-to-string reason)))))
	      (dprint :warn "Websocket closed with code: ~a~%Reason: ~a~%" code reason)
	      (cargo-send >status-close> (list reason code)
			  (lc:id (user bot)))))))
