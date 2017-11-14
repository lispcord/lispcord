(in-package :lispcord.classes.message)

(deftype message-type () `(integer 0 7))

(defclass attachement ()
  ((id        :initarg :id
	      :type snowflake)
   (filename  :initarg :file
	      :type string)
   (size      :initarg :size
	      :type fixnum)
   (url       :initarg :url
	      :type string)
   (proxy-url :initarg :proxy
	      :type string)
   (height    :initarg :height
	      :type (or null fixnum))
   (width     :initarg :width
	      :type (or null fixnum))))

(defmethod from-json ((c (eql :attachement)) (table hash-table))
  (instance-from-table (table 'attachement)
    :id "id"
    :file "filename"
    :size "size"
    :url "url"
    :proxy-url "proxy_url"
    :height "height"
    :width "width"))

(defmethod %to-json ((a attachement))
  (with-object
    (write-key-value "id" (!! a id))
    (write-key-value "filename" (!! a filename))
    (write-key-value "size" (!! a size))
    (write-key-value "url" (!! a url))
    (write-key-value "proxy_url" (!! a proxy-url))
    (write-key-value "height" (!! a height))
    (write-key-value "width" (!! a width))))

(defclass reaction ()
  ((count :initarg :count
	  :type fixnum)
   (me    :initarg :me
	  :type t)
   (emoji :initarg :emoji
	  :type emoji)))

(defmethod from-json ((c (eql :reaction)) (table hash-table))
  (instance-from-table (table 'reaction)
    :count "count"
    :me "me"
    :emoji (from-json :emoji (gethash "emoji" table))))

(defclass message ()
  ((id            :initarg :id
		  :type snowflake)
   (channel-id    :initarg :channel-id
		  :type snowflake)
   (author        :initarg :author
		  :type user)
   (content       :initarg :content
		  :type string)
   (timestamp     :initarg :timestamp
		  :type string)
   (edited-at     :initarg :edited-at
		  :type (or null string))
   (tts           :initarg :tts
		  :type t)
   (mention-all   :initarg :mention-all
		  :type t)
   (mentions      :initarg :mentions
		  :type (vector user))
   (mention-roles :initarg :mention-roles
		  :type (vector role))
   (attachements  :initarg :attachements
		  :type (vector attachement))
   (embeds        :initarg :embeds
		  :type (vector embeds))
   (reactions     :initarg :reactions
		  :type (or null (vector reaction)))
   (nonce         :initarg :nonce
		  :type (or null snowflake))
   (pinned        :initarg :pinned
		  :type t)
   (webhook-id    :initarg :webhook-id
		  :type (or null snowflake))
   (type          :initarg :type
		  :type message-type)))

(defmethod from-json ((c (eql :message)) (table hash-table))
  (instance-from-table (table 'message)
    :id "id"
    :channel-id "channel_id"
    :author (from-json :user (gethash "author" table))
    :content "content"
    :timestamp "timestamp"
    :edited-at "edited_timestamp"
    :tts "tts"
    :mention-all "mention_everyone"
    :mentions (map 'vector (curry #'from-json :user)
		   (gethash "mentions" table))
    :mention-roles (map 'vector (curry #'from-json :role)
			(gethash "mention_roles" table))
    :attachements (map 'vector (curry #'from-json :attachement)
		       (gethash "attachements" table))
    :embeds (map 'vector (curry #'from-json :embed)
		 (gethash "embeds" table))
    :reactions (map 'vector (curry #'from-json :reaction)
		    (gethash "embeds" table))
    :nonce "nonce"
    :pinned "pinned"
    :webhook-id "webhook_id"
    :type "type"))

(defmethod %to-json ((m message))
  (with-object
    (write-key-value "id" (!! m id))
    (write-key-value "channel_id" (!! m channel-id))
    (write-key-value "author" (!! m author))
    (write-key-value "content" (!! m content))
    (write-key-value "timestampt" (!! m timestamp))
    (write-key-value "edited_timestamp" (!! m edited_at))
    (write-key-value "tts" (!! m tts))
    (write-key-value "mention_everyone" (!! m mention-all))
    (write-key-value "mentions" (!! m mentions))
    (write-key-value "mention_roles" (!! m mention-roles))
    (write-key-value "attachements" (!! m attachements))
    (write-key-value "embeds" (!! m embeds))
    (write-key-value "reactions" (!! m reactions))
    (write-key-value "nonce" (!! m nonce))
    (write-key-value "pinned" (!! m pinned))
    (write-key-value "webhook_id" (!! m webhook-id))
    (write-key-value "type" (!! m type))))
