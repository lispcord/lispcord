
(in-package :lispcord.http)

(defmethod from-id (id (g (eql :guild)) &optional (bot *client*))
  (alexandria:if-let ((cached (getcache-id id :guild)))
    cached
    (cache :guild
           (discord-req (str-concat "guilds/" id)
                        :bot bot))))

(defmethod erase ((g lc:guild) &optional (bot *client*))
  (discord-req (str-concat "guilds/" (lc:id g))
               :bot bot
               :type :delete))

(defun get-channels (guild &optional (bot *client*))
  (declare (type (or snowflake lc:guild) guild))
  (let ((g (if (typep guild 'lc:guild) (lc:id guild) guild)))
    (map '(vector lc:channel)
         (curry #'cache :channels)
         (discord-req (str-concat "guilds/" g "/channels")
                      :bot bot))))

(defmethod create ((c lc:partial-channel) (g lc:guild)
                   &optional (bot *client*))
  (cache :channel
         (discord-req (str-concat "guilds/" (lc:id g) "/channels")
                      :bot bot
                      :type :post
                      :content (to-json c))))


(defmethod from-id ((u lc:user) (g lc:guild) &optional (bot *client*))
  (let ((m? (find-if (lambda (e)
                       (funcall optimal-id-compare
                                (lc:id (lc:user e))
                                (lc:id u)))
                     (lc:members g))))
    (if m?
        m?
        (let ((m (from-json :member
                            (discord-req
                             (str-concat "guilds/" (lc:id g)
                                         "/members/" (lc:id u))
                             :bot bot))))
          (setf (gethash "guild_id" m) (lc:id g))))))

(defun get-members (guild &key (limit 1) after (bot *client*))
  (declare (type (or snowflake lc:guild) guild))
  (let ((g (if (typep guild 'lc:guild) (lc:id guild) guild))
        (params (append (if limit `(("limit" . ,(to-string limit))))
                        (if after `(("after" . ,(to-string after)))))))
    (map '(vector lc:member)
         (curry #'from-json :g-member)
         (discord-req (str-concat "guilds/" g "/members")
                      :parameters params
                      :bot bot))))


(defmethod edit ((m lc:member) (g lc:guild) &optional (bot *client*))
  (discord-req (str-concat "guilds/" (lc:id g)
                           "/members/" (lc:id (lc:user m)))
               :bot bot
               :type :patch
               :content (let ((o nil))
                          (when (slot-boundp m 'lc:nick) (push (cons "nick" (lc:nick m)) o))
                          (when (slot-boundp m 'lc:roles) (push (cons "roles" (lc:roles m)) o))
                          (when (slot-boundp m 'lc:mutep) (push (cons "mute" (lc:mutep m)) o))
                          (when (slot-boundp m 'lc:deafp) (push (cons "deaf" (lc:deafp m)) o))
                          (jmake o))))

(defun move-member (member channel &optional (bot *client*))
  (declare (type lc:member member)
           (type (or snowflake lc:guild-channel) channel))
  (let ((m (lc:id (lc:user member)))
        (g (lc:guild-id member))
        (c (if (typep channel 'lc:channel) (lc:id channel) channel)))
    (discord-req (str-concat "guilds/" g
                             "/members/" m)
                 :bot bot
                 :type :patch
                 :content (jmake `(("channel_id" . ,c))))))

(defun set-nick (nick guild &optional (bot *client*))
  (declare (type (or snowflake lc:guild) guild))
  (let ((g (if (typep guild 'lc:guild) (lc:id guild) guild)))
    (gethash "nick" (discord-req
                     (str-concat "/guilds/" g "/members/@me/nick")
                     :bot bot
                     :type :patch
                     :content (str-concat "{\"nick\":\"" nick "\"}")))))

(defmethod create ((r lc:role) (m lc:member) &optional (bot *client*))
  (discord-req (str-concat "guilds/" (lc:guild-id m)
                           "/members/" (lc:id (lc:user m))
                           "/roles/" (lc:id r))
               :bot bot
               :type :put
               :content "{}"))

(defun erase-role (role member &optional (bot *client*))
  (declare (type (or snowflake lc:role) role)
           (type lc:member member))
  (let ((r (if (typep role 'lc:role) (lc:id role) role))
        (m (lc:id (lc:user member)))
        (g (lc:guild-id member)))
    (discord-req (str-concat "guilds/" g
                             "/members/" m
                             "/roles/" r)
                 :bot bot
                 :type :delete)))


(defmethod erase ((m lc:member) &optional (bot *client*))
  (discord-req (str-concat "guilds/" (lc:guild-id m)
                           "/members/" (lc:id (lc:user m)))
               :bot bot
               :type :delete))

(defun get-bans (guild &optional (bot *client*))
  (declare (type (or snowflake lc:guild) guild))
  (let ((g (if (typep guild 'lc:guild) (lc:id guild) guild)))
    (discord-req (str-concat "guilds/" g "/bans")
                 :bot bot)))

(defun ban (user guild &optional delete (bot *client*))
  (declare (type (or snowflake lc:user) user)
           (type (or snowflake lc:guild) guild))
  (let ((u (if (typep user 'lc:user) (lc:id user) user))
        (g (if (typep guild 'lc:guild) (lc:id guild) guild)))
    (discord-req (str-concat "guilds/" g "/bans/" u
                             (if delete
                                 (str-concat "?delete-message-days="
                                             (to-string delete))
                                 ""))
                 :bot bot
                 :type :put
                 :content "{}")))

(defun unban (user guild &optional (bot *client*))
  (declare (type (or snowflake lc:user) user)
           (type (or snowflake lc:guild) guild))
  (let ((u (if (typep user 'lc:user) (lc:id user) user))
        (g (if (typep guild 'lc:guild) (lc:id guild) guild)))
    (discord-req (str-concat "guilds/" g "/bans/" u)
                 :bot bot
                 :type :delete)))

(defun get-roles (guild &optional (bot *client*))
  (declare (type (or snowflake lc:guild) guild))
  (let ((g (if (typep guild 'lc:guild) (lc:id guild) guild)))
    (map '(vector lc:role)
         (curry #'cache :role)
         (discord-req (str-concat "guilds/" g "/roles")
                      :bot bot))))


(defmethod create ((r lc:partial-role) (g lc:guild)
                   &optional (bot *client*))
  (cache :role (discord-req (str-concat "guilds/" (lc:id g) "/roles")
                            :bot bot
                            :type :post
                            :content (to-json r))))

(defmethod edit ((r lc:partial-role) (role lc:role)
                 &optional (bot *client*))
  (cache :role (discord-req (str-concat "guilds/" (lc:guild-id role)
                                        "/roles/" (lc:id role))
                            :bot bot
                            :type :patch
                            :content (to-json r))))

(defmethod edit ((r lc:role) (role lc:role)
                 &optional (bot *client*))
  (cache :role
         (discord-req
          (str-concat "guilds/" (lc:guild-id role)
                      "/roles/" (lc:id role))
          :bot bot
          :type :patch
          :content (jmake `(("name" . ,(lc:name r))
                            ("permissions" . (lc:permissions r))
                            ("color" . (lc:color r))
                            ("hoist" . (lc:hoistp r))
                            ("mentionable" . (lc:mentionablep r)))))))

(defmethod erase ((r lc:role) &optional (bot *client*))
  (discord-req (str-concat "guilds/" (lc:guild-id r)
                           "/roles/" (lc:id r))
               :bot bot
               :type :delete))


(defmethod from-id (id (c (eql :role))
                    &optional (bot *client*))
  (declare (ignore bot))
  (getcache-id id :role))
