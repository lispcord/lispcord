(in-package :lispcord.http)

(defclass new-guild ()
  ((name :initarg :name)
   (region :initarg :region)
   (icon :initarg :icon)
   (verify-l :initarg :verify-l)
   (notify-l :initarg :notify-l)
   (roles :initarg :roles)
   (channels :initarg :channels)))

(defclass part-chnl ()
  ((name :initarg :name)
   (type :initarg :type)))

(defclass plac-role (lc:role)
  ((id :initarg :id
       :type fixnum
       :accessor lc:id)))

(defmethod from-id (id (g (eql :guild)) &optional (bot *client*))
  (cache :guild
	 (discord-req (str-concat "guilds/" id)
		      :bot bot)))

(defmethod erase ((g lc:guild) &optional (bot *client*))
  (discord-req (str-concat "guilds/" (lc:id g))
	       :bot bot
	       :type :delete))

(defun get-channels (guild &optional (bot *client*))
  (declare (type (or snowflake lc:guild) guild))
  (let ((g (if (typep guild 'lc:guild) (lc:id guild) guild)))
    (mapvec (curry #'cache :channels)
	    (discord-req (str-concat "guilds/" g "/channels")
			 :bot bot))))

(defmethod create ((c new-chnl) (g lc:guild) &optional (bot *client*))
  (cache :channel
	 (discord-req (str-concat "guilds/" (lc:id g) "/channels")
		      :bot bot
		      :type :post
		      :content (to-json c))))


(defmethod from-id ((u lc:user) (g lc:guild) &optional (bot *client*))
  (from-json :member
	     (discord-req (str-concat "guilds/" (lc:id g)
				      "/members/" (lc:id u))
			  :bot bot)))

(defun get-members (guild &key (limit 1) after (bot *client*))
  (declare (type (or snowflake lc:guild) guild))
  (let ((g (if (typep guild 'lc:guild) (lc:id guild) guild))
	(params (append (if limit `(("limit" . ,(to-string limit))))
			(if after `(("after" . ,(to-string after)))))))
    (mapvec (curry #'from-json :g-member)
	    (discord-req (str-concat "guilds/" g "/members")
			 :parameters params
			 :bot bot))))




