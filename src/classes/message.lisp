(in-package :lispcord.classes)


(deftype message-type () `(integer 0 7))

(defclass attachement ()
  ((id        :initarg :id
	      :type snowflake
	      :accessor id)
   (filename  :initarg :file
	      :type string
	      :accessor filename)
   (size      :initarg :size
	      :type fixnum
	      :accessor size)
   (url       :initarg :url
	      :type string
	      :accessor url)
   (proxy-url :initarg :proxy
	      :type string
	      :accessor proxy-url)
   (height    :initarg :height
	      :type (or null fixnum)
	      :accessor height)
   (width     :initarg :width
	      :type (or null fixnum)
	      :accessor width)))

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
    (write-key-value "id" (id a))
    (write-key-value "filename" (filename a))
    (write-key-value "size" (size a))
    (write-key-value "url" (url a))
    (write-key-value "proxy_url" (proxy-url a))
    (write-key-value "height" (height a))
    (write-key-value "width" (width a))))

(defclass reaction (msg-obj)
  ((count :initarg :count
	  :type fixnum
	  :accessor count)
   (me    :initarg :me
	  :type t
	  :accessor mep)
   (emoji :initarg :emoji
	  :type emoji
	  :accessor emoji)))

(defmethod from-json ((c (eql :reaction)) (table hash-table))
  (instance-from-table (table 'reaction)
    :count "count"
    :me "me"
    :emoji (from-json :emoji (gethash "emoji" table))))

(defclass message (msg-obj)
  ((id            :initarg :id
		  :type snowflake
		  :accessor id)
   (channel-id    :initarg :channel-id
		  :type snowflake
		  :accessor channel-id)
   (author        :initarg :author
		  :type user
		  :accessor user)
   (content       :initarg :content
		  :type string
		  :accessor content)
   (timestamp     :initarg :timestamp
		  :type string
		  :accessor timestamp)
   (edited-at     :initarg :edited-at
		  :type (or null string)
		  :accessor editedp)
   (tts           :initarg :tts
		  :type t
		  :accessor tts-p)
   (mention-all   :initarg :mention-all
		  :type t
		  :accessor mention-all-p)
   (mentions      :initarg :mentions
		  :type (vector user)
		  :accessor mentions)
   (mention-roles :initarg :mention-roles
		  :type (vector role)
		  :accessor mention-roles)
   (attachements  :initarg :attachements
		  :type (vector attachement)
		  :accessor attachements)
   (embeds        :initarg :embeds
		  :type (vector embed)
		  :accessor embeds)
   (reactions     :initarg :reactions
		  :type (or null (vector reaction))
		  :accessor reactions)
   (nonce         :initarg :nonce
		  :type (or null snowflake)
		  :accessor nonce)
   (pinned        :initarg :pinned
		  :type t
		  :accessor pinnedp)
   (webhook-id    :initarg :webhook-id
		  :type (or null snowflake)
		  :accessor webhook-id)
   (type          :initarg :type
		  :type message-type
		  :accessor type)))

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
    (write-key-value "id" (id m))
    (write-key-value "channel_id" (channel-id m))
    (write-key-value "author" (author m))
    (write-key-value "content" (content m))
    (write-key-value "timestampt" (timestamp m))
    (write-key-value "edited_timestamp" (editedp m))
    (write-key-value "tts" (tts-p m))
    (write-key-value "mention_everyone" (mention-all-p m))
    (write-key-value "mentions" (mentions m))
    (write-key-value "mention_roles" (mention-roles m))
    (write-key-value "attachements" (attachements m))
    (write-key-value "embeds" (embeds m))
    (write-key-value "reactions" (reactions m))
    (write-key-value "nonce" (nonce m))
    (write-key-value "pinned" (pinnedp m))
    (write-key-value "webhook_id" (webhook-id m))
    (write-key-value "type" (type m))))
