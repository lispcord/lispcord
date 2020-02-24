(in-package :lispcord.http)

(defmethod from-id (id (c (eql :user)) &optional (bot *client*))
  (or (getcache-id id 'lc:user)
      (cache 'lc:user (discord-req (str-concat "users/" id)
                                   :bot bot))))


(defun current-user (&optional (bot *client*))
  (cache 'lc:user (discord-req "users/@me" :bot bot)))

(defmethod edit ((u lc:user) (user lc:user) &optional (bot *client*))
  (cache 'lc:user
         (discord-req
          (str-concat "users/" (lc:id user))
          :bot bot
          :type :patch
          :content (jmake
                    (list (cons "username" (lc:name u))
                          (cons "avatar" (lc:avatar u)))))))

(defun leave (guild &optional (bot *client*))
  (declare (type (or snowflake lc:base-guild) guild))
  (let ((g (if (typep guild 'lc:base-guild) (lc:id guild) guild)))
    (discord-req (str-concat "users/@me/guilds/" g)
                 :bot bot
                 :type :delete)))

(defun get-dms (&optional (bot *client*))
  (mapvec (curry #'cache 'lc:channel)
          (discord-req (str-concat "users/@me/channels")
                       :bot bot)))

(defun create-dm (user &optional (bot *client*))
  (declare (type (or snowflake lc:user) user))
  (let ((u (if (typep user 'lc:user) (lc:id user) user)))
    (cache 'lc:channel (discord-req
                        (str-concat "users/@me/channels")
                        :bot bot
                        :type :post
                        :content (jmake `(("recipient_id" . ,u)))))))

(defmethod create ((s string) (u lc:user) &optional (bot *client*))
  (if (getcache-id (lc:id u) 'lc:channel)
      (create (lc:make-message s) (getcache-id (lc:id u) 'lc:channel) bot)
      (create (lc:make-message s) (create-dm u bot) bot)))

(defmethod create ((s lc:partial-message) (u lc:user)
                   &optional (bot *client*))
  (if (getcache-id (lc:id u) 'lc:channel)
      (create s (getcache-id (lc:id u) 'lc:channel) bot)
      (create s (create-dm u bot) bot)))

