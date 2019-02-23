(in-package lispcord.http)

(defmethod from-id (id (g lc:guild) &optional (bot *client*))
  (if (getcache-id id :emoji)
      (getcache-id id :emoji)
      (cache :emoji
             (discord-req (str-concat "guilds/" (lc:id g)
                                      "/emojis/" id)
                          :bot bot))))

(defun get-emojis (guild &optional (bot *client*))
  (declare (type (or snowflake lc:guild) guild))
  (let ((g (if (typep guild 'lc:guild) (lc:id guild) guild)))
    (mapvec (curry #'cache :emoji)
            (discord-req (str-concat "guilds/" g
                                     "/emojis")
                         :bot bot))))


(defmethod create ((e lc:partial-emoji) (g lc:guild)
       &optional (bot *client*))
  (cache :emoji
   (discord-req (str-concat "guilds/" (lc:id g)
                            "/emojis")
                :bot bot
                :type :post
                :content (to-json e))))

(defmethod edit ((e lc:partial-emoji) (g lc:guild)
     &optional (bot *client*))
  (cache :emoji
   (discord-req (str-concat "guilds/" (lc:id g) "/emojis")
                :bot bot
                :type :patch
                :content (jmake
                          `(("name" . ,(slot-value e 'name))
                            ("roles" . ,(slot-value e 'roles)))))))

(defmethod edit ((e lc:emoji) (g lc:guild) &optional (bot *client*))
  (cache :emoji
   (discord-req (str-concat "guilds/" (lc:id g) "/emojis")
                :bot bot
                :type :patch
                :content (jmake
                    `(("name" . ,(slot-value e 'name))
                      ("roles" . ,(slot-value e 'roles)))))))

(defun erase-emoji (emoji guild &optional (bot *client*))
  (declare (type (or snowflake lc:guild) guild)
     (type (or snowflake lc:emoji) emoji))
  (let ((e (if (typep emoji 'lc:emoji) (lc:id emoji) emoji))
  (g (if (typep guild 'lc:guild) (lc:id guild) guild)))
    (discord-req (str-concat "guilds/" (lc:id g) "/emojis/" (lc:id e))
                 :bot bot
                 :type :delete)))
