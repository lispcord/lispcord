(in-package :lispcord.gateway)

(defvar *gateway-url* nil)

(defun refresh-gateway-url ()
  (doit  (get-rq "gateway")
         (gethash "url" it)
         (:! v:debug :lispcord.gateway "Gateway-url: ~a" it)
         (str-concat it +api-suffix+)
         (setf *gateway-url* it)))





(defun send-payload (bot &key op data)
  (doit (jmake `(("op" . ,op) ("d" . ,data)))
        (:! v:debug :lispcord.gateway "Send payload: ~a" it)
        (wsd:send (bot-conn bot) it)))

(defun make-status (bot status game afk)
  (let ((since (when afk (if (bot-afk-since bot)
                             (bot-afk-since bot)
                             (setf (bot-afk-since bot) (since-unix-epoch))))))
    `(("since" . ,since)
      ("game" . ,game)
      ("afk" . ,afk)
      ("status" . ,status))))

(defun send-identify (bot)
  (v:info :lispcord.gateway "Send identify for ~a" (bot-token bot))
  (send-payload bot
                :op 2
                :data `(("token" . ,(lispcord.core:bot-auth bot))
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
  (v:info :lispcord.gateway "Resuming connection for session ~a..."
          (bot-session-id bot))
  (send-payload bot
                :op 6
                :data `(("token" . ,(bot-token bot))
                        ("session_id" . ,(bot-session-id bot))
                        ("seq" . ,(bot-seq bot)))))


(defun on-ready (bot payload)
  (v:info :lispcord.gateway "Ready payload received; Session-id: ~a"
          (gethash "session_id" payload))
  (setf (bot-session-id bot) (gethash "session_id" payload))
  (setf (bot-user bot) (cache :user (gethash "user" payload)))
  ;;dispatch event
  (dispatch-event :on-ready (list (from-json :ready payload)) bot))




(defun send-status-update (bot &optional game (status :online))
  (send-payload bot
                :op 3
                :data (make-status bot status game nil)))




(defun send-heartbeat (bot)
  (send-payload bot
                :op 1
                :data (bot-seq bot)))

(defun make-heartbeat-thread (bot seconds
                              &optional (stream *error-output*))
  (declare (type rational seconds))
  (v:info :lispcord.gateway "Initiating heartbeat every ~d seconds" seconds)
  (make-thread (lambda ()
                 (let ((*error-output* stream))
                   (loop
                      (v:debug :lispcord.gateway "Dispatching heartbeat!")
                      (send-heartbeat bot)
                      (sleep seconds))))))






(defun on-emoji-update (data bot)
  (with-table (data emojis "emojis"
                    id "guild_id")
    (let ((g (cache :guild (plist-hash-table `("id" ,id "emojis" ,emojis)
                                             :test #'equal))))
      (dispatch-event :on-emoji-update
                      (list (lc:emojis g) g)
                      bot))))


(defun on-member-remove (data bot)
  (let* ((user (getcache-id (parse-snowflake
                             (gethash "id" (gethash "user" data)))
                            :user))
         (g-id (parse-snowflake (gethash "guild_id" data)))
         (g (getcache-id g-id :guild)))
    (setf (lc:members g)
          (vecrem (lambda (e) (eq user (lc:user e))) (lc:members g)))
    (dispatch-event :on-member-remove
                    (list user g)
                    bot)))

(defun on-member-add (data bot)
  (let ((member (from-json :g-member data))
        (g (getcache-id (parse-snowflake (gethash "guild_id" data))
                        :guild)))
    (setf (lc:members g) (vec-extend member (lc:members g)))
    (dispatch-event :on-member-add (list member g) bot)))

(defun on-members-chunk (data bot)
  (with-table (data id "guild_id" members "members")
    (mapf members (m)
      (setf (gethash "guild_id" m) id)
      (on-member-add m bot))))

(defun on-member-update (data bot)
  (let ((g (getcache-id (parse-snowflake (gethash "guild_id" data))
                        :guild))
        (member (from-json :g-member data)))
    (nsubstitute-if member
                    (lambda (m) (eq (lc:user m) (lc:user member)))
                    (lc:members g))
    (dispatch-event :on-member-update (list member g) bot)))

(defun on-role-add (data bot)
  (let ((role (cache :role (gethash "role" data)))
        (g (getcache-id (parse-snowflake (gethash "guild_id" data))
                        :guild)))
    (setf (lc:guild-id role) (parse-snowflake (gethash "guild_id" data)))
    (setf (lc:roles g) (vec-extend role (lc:roles g)))
    (dispatch-event :on-role-create (list role g) bot)))

(defun on-role-update (data bot)
  (let ((role (cache :role (gethash "role" data)))
        (g (getcache-id (parse-snowflake (gethash "guild_id" data))
                        :guild)))
    (dispatch-event :on-role-update (list role g) bot)))

(defun on-role-delete (data bot)
  (with-table (data guild-id "guild_id" role-id "role_id")
    (let* ((g-id (parse-snowflake guild-id))
           (r-id (parse-snowflake role-id))
           (g (getcache-id g-id :guild)))
      (decache-id r-id :role)
      (setf (lc:roles g)
            (vecrem (lambda (e)
                      (funcall optimal-id-compare r-id (lc:id e)))
                    (lc:roles g)))
      (dispatch-event :on-role-delete (list r-id g-id) bot))))


(defun on-channel-create (data bot)
  (let ((c (cache :channel data)))
    (when (typep c 'lc:guild-channel)
      (let ((g (getcache-id (lc:guild-id c) :guild)))
        (setf (lc:channels g) (vec-extend c (lc:channels g)))))
    (dispatch-event :on-channel-create (list c) bot)))

(defun on-channel-delete (data bot)
  (let ((c (cache :channel data)))
    (when (typep c 'lc:guild-channel)
      (let ((g (getcache-id (lc:guild-id c) :guild)))
        (setf (lc:channels g)
              (vecrem (lambda (e) (eq e c)) (lc:channels g)))))
    (decache-id (lc:id c) :channel)
    (dispatch-event :on-channel-delete (list c) bot)))

(defun on-channel-pin-update (data bot)
  (let ((id (parse-snowflake (gethash "channel_id" data))))
    (dispatch-event :on-pin-update
                    (list (getcache-id id :channel)
                          (gethash "last_pin_timestamp" data))
                    bot)))


(defun on-guild-ban (data bot kind)
  (let ((u (cache :user (gethash "user" data)))
        (g (getcache-id (parse-snowflake (gethash "guild_id" data))
                        :guild)))
    (dispatch-event kind (list u g) bot)))


(defun on-reaction (data bot kind)
  (let ((emoji (if (gethash "id" (gethash "emoji" data))
                   (cache :emoji (gethash "emoji" data))
                   (gethash "emoji" data)))
        (user (getcache-id (parse-snowflake (gethash "user_id" data))
                           :user))
        (channel (getcache-id (parse-snowflake (gethash "channel_id" data))
                              :channel))
        (mid (parse-snowflake (gethash "message_id" data))))
    (dispatch-event kind (list emoji mid user channel) bot)))


(defun on-presence (data bot)
  (when-let ((u (getcache-id (parse-snowflake
                               (gethash "id" (gethash "user" data)))
                              :user)))
    (v:debug :lispcord.gateway "User uncached: ~a"
            (gethash "id" (gethash "user" data)))
    (setf (lc:status u) (gethash "status" data))
    (setf (lc:game u) (from-json :game (gethash "game" data))))
  (dispatch-event :on-presence-update
                  (list (from-json :presence data))
                  bot))

(defun on-typing-start (data bot)
  (let ((c (getcache-id (parse-snowflake (gethash "channel_id" data))
                        :channel))
        (u (getcache-id (parse-snowflake (gethash "user_id" data))
                        :user))
        (ts (gethash "timestamp" data)))
    (dispatch-event :on-typing-start (list u c ts) bot)))

;; opcode 0
(defun on-dispatch (bot msg)
  (let ((event (gethash "t" msg))
        (seq (gethash "s" msg))
        (data (gethash "d" msg)))
    (setf (bot-seq bot) seq)
    (v:info :lispcord.gateway "[Event] ~a" event)
    (v:debug :lispcord.gateway "[Payload] ~a" msg)
    (switch (event :test #'string=)
      ;; on handshake
      ("READY"
       (on-ready bot data))                           

      ;; on resume
      ("RESUMED"
       (dispatch-event :on-status-resumed nil bot))

      ;; someone starts typing
      ("TYPING_START"
       (on-typing-start data bot))

      ("USER_UPDATE"
       (dispatch-event :on-user-update (list (cache :user data)) bot))

      ;; channel made known
      ("CHANNEL_CREATE"
       (on-channel-create data bot))

      ("CHANNEL_UPDATE"
       (dispatch-event :on-channel-update
                       (list (cache :channel data))
                       bot))

      ("CHANNEL_DELETE"
       (on-channel-delete data bot))

      ("CHANNEL_PINS_UPDATE"
       (on-channel-pin-update data bot))

      ;; guild made known
      ("GUILD_CREATE"
       (dispatch-event :on-guild-create
                       (list (cache :guild data))
                       bot))

      ("GUILD_UPDATE"
       (dispatch-event :on-guild-update
                       (list (cache :guild data))
                       bot))

      ("GUILD_DELETE"
       (let ((g (cache :guild data)))
         (dispatch-event :on-guild-delete (list g) bot)
         (decache-id (lc:id g) :guild)))

      ("GUILD_BAN_ADD"
       (on-guild-ban data bot :on-member-ban))

      ("GUILD_BAN_REMOVE"
       (on-guild-ban data bot :on-member-unban))

      ("GUILD_EMOJIS_UPDATE"
       (on-emoji-update data bot))

      ("GUILD_INTEGRATIONS_UPDATE"
       (dispatch-event :on-integrations-update
                       (list (getcache-id
                              (parse-snowflake (gethash "guild_id" data))
                              :guild))
                       bot))

      ("GUILD_MEMBER_ADD"
       (on-member-add data bot))

      ("GUILD_MEMBER_REMOVE"
       (on-member-remove data bot))

      ("GUILD_MEMBER_UPDATE"
       (on-member-update data bot))

      ("GUILD_MEMBERS_CHUNK"
       (on-members-chunk data bot))

      ("GUILD_ROLE_CREATE"
       (on-role-add data bot))

      ("GUILD_ROLE_DELETE"
       (on-role-delete data bot))

      ("GUILD_ROLE_UPDATE"
       (on-role-update data bot))

      ;; received new message
      ("MESSAGE_CREATE"
       (dispatch-event :on-message-create
                       (list (from-json :message data))
                       bot))

      ;; a message is edited // might need special parsing here
      ("MESSAGE_UPDATE"
       (dispatch-event :on-message-update
                       (list data)
                       bot))

      ;; a message is deleted
      ("MESSAGE_DELETE"
       (dispatch-event :on-message-delete
                       (list (parse-snowflake (gethash "id" data))
                             (getcache-id
                              (parse-snowflake (gethash "channel_id" data))
                              :channel))
                       bot))

      ("MESSAGE_DELETE_BULK"
       (let ((ids (mapvec #'parse-snowflake (gethash "ids" data)))
             (c (getcache-id
                 (parse-snowflake (gethash "channel_id" data))
                 :channel)))
         (dispatch-event :on-message-purge (list c ids) bot)))

      ("MESSAGE_REACTION_ADD"
       (on-reaction data bot :on-reaction-add))

      ("MESSAGE_REACTION_REMOVE"
       (on-reaction data bot :on-reaction-remove))

      ("MESSAGE_REACTION_REMOVE_ALL"
       (let ((c (getcache-id
                 (parse-snowflake (gethash "channel_id" data))
                 :channel))
             (mid (parse-snowflake (gethash "message_id" data))))
         (dispatch-event :on-reaction-purge
                         (list c mid)
                         bot)))
      
      ;; someone updates their presence
      ("PRESENCE_UPDATE"
       (on-presence data bot))

      ;; unrecognised event!
      (:else
       (v:warn :lispcord.gateway "unrecognised event! ~a" event)))))


;; opcode 10
(defun on-hello (bot msg)
  (let ((heartbeat-interval (gethash "heartbeat_interval" (gethash "d" msg))))
    (declare (type (unsigned-byte 32) heartbeat-interval))
    (v:debug :lispcord.gateway "Heartbeat Inverval: ~a" heartbeat-interval)
    (setf (bot-heartbeat-thread bot)
          (make-heartbeat-thread bot (/ heartbeat-interval 1000)))
    (if (bot-session-id bot)
        (send-resume bot)
        (send-identify bot))))




(defun disconnect (bot &optional reason code)
  (let ((out *error-output*))
    (bt:join-thread
     (bt:make-thread
      (lambda ()
        (let ((*error-output* out))
          (v:info :lispcord.gateway "~a disconnecting..."
                  (if (bot-user bot) (lc:name (bot-user bot))))
          (wsd:close-connection (bot-conn bot) reason code)
          (setf (bot-seq bot) 0)
          (setf (bot-session-id bot) nil)
          (bt:destroy-thread (bot-heartbeat-thread bot))))))))

(defun cleanup (bot)
  (v:warn :lispcord.gateway "Cleanup loop engaged! Bot: ~a" (lc:name (bot-user bot)))
  (disconnect bot)
  (sleep (random 5))
  (connect bot))

(defun reconnect (bot &optional reason code)
  (v:warn :lispcord.gateway "Attempting to reconnect! Bot: ~a" (lc:name (bot-user bot)))
  (wsd:close-connection (bot-conn bot) reason code)
  (setf (bot-conn bot) nil)
  (setf (bot-heartbeat-thread bot) nil))


;; receive message from websocket and dispatch to handler
(defun on-recv (bot msg)
  (let ((op (gethash "op" msg)))
    (case op
      (0  (with-simple-restart (abort "Skip handling this event")
            (on-dispatch bot msg))) ;Dispatch Event
      (1  (send-heartbeat bot))  ;Requests Heartbeat
      (7  (reconnect bot))   ;Requests Reconnect
      (9  (cleanup bot))         ;Invalid Sessions Event
      (10 (on-hello bot msg))    ;Hello Event
      (11 (v:debug :lispcord.gateway "Received Heartbeat ACK"))
      (T ;; not sure if this should be an error to the user or not?
       (v:error :lispcord.gateway "Received invalid opcode! ~a" op)))))


(defun connect (bot)
  (assert (typep bot 'bot))
  (unless *gateway-url* (refresh-gateway-url))
  (setf (bot-conn bot) (wsd:make-client *gateway-url*))
  (wsd:start-connection (bot-conn bot))
  
  (wsd:on :open (bot-conn bot)
          (lambda ()
            (v:info :lispcord.gateway "Connected!")))
  
  (wsd:on :message (bot-conn bot)
          (lambda (message)
            (on-recv bot (jparse message))))
  
  (wsd:on :error (bot-conn bot)
          (lambda (error)
            (v:error :lispcord.gateway "Websocket error: ~a" error)
            (refresh-gateway-url)))
  
  (wsd:on :close (bot-conn bot)
          (lambda (&key code reason)
            (let ((reason (typecase reason
                            (null nil)
                            (string reason)
                            (vector (babel:octets-to-string reason)))))
              (v:warn :lispcord.gateway "Websocket closed with code: ~a Reason: ~a" code reason)
              (when (bot-session-id bot)
                (disconnect bot reason code))
              (dispatch-event :on-close
                              (list reason code)
                              bot)))))
