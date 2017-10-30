;;; The Channel & overwrite objects

(in-package :lispcord.payloads)

(defclass overwrites ()
  ((id    :initarg :id
          :reader  user-id
          :type    string)
   (dtype  :initarg :dtype
	  :reader  dtype
	  :type    string)
   (allow :initarg :allow
	  :reader  allow
	  :type    fixnum)
   (deny  :initarg :deny
	  :reader  deny
	  :type    fixnum))
  (:documentation "Sets the channel specific permissions for a user"))

(defclass channel ()
  ((id   :initarg :id
         :reader  id
         :type    string)
   (dtype :initarg :dtype
	 :reader  dtype
	 :type    (integer 0 4)))
  (:documentation "The basic channel object"))


(defclass guild-channel (channel)
  ((guild-id              :initarg :guild-id
	                  :reader  guild-id?     ; could be nil!
	                  :type    (or null string))
   (cposition              :initarg :cposition
	                  :reader  cposition?     ; could be nil!
	                  :type    (or null fixnum))
   (permission-overwrites :initarg :permission-overwrites
			  :reader  permissions?  ; could be nil!
			  :type    (vector overwrites))
   (name                  :initarg :name
	                  :reader  name?         ; could be nil!
	                  :type    (or null string))
   (parent-id             :initarg :parent
			  :reader  parent?       ;could be nil!
			  :type    (or null string)))
  (:documentation "Guild channels are bound to a specific server"))

(defclass text-channel (guild-channel)
  ((topic           :initarg :topic
	            :reader  topic?        ; could be nil!
	            :type    (or null string))
   (nsfw            :initarg :nsfw
		    ; could be nil, but it's a bool anyways
	            :reader  nsfw?  
	            :type    (or null t))
   (last-message-id :initarg :last-message
		    :reader  last-message? ; could be nil!
		    :type    (or null string)))
  (:documentation "Note that DM-channels are handled seperately"))

(defclass voice-channel (guild-channel)
  ((bitrate    :initarg :bitrate
		:reader  bitrate?	; could be nil!
		:type    (or null fixnum))
   (user-limit :initarg :user-limit
	       :reader  user-limit? ; could be nil!
	       :type    (or null fixnum))))

(defclass category (guild-channel)
  ((nsfw :initarg :nsfw
	  :reader  nsfw?	 ; could be nil, but is a bool anyways
	  :type    (or null t))))

(defclass dm-channel (channel)
  ((last-message-id :initarg :last-message
		    :reader  last-message? ; could be nil!
		    :type    (or null string))
   (recipients      :initarg :recipients
	            :reader  recipients?
	            :type    (or null (vector user))))
  (:documentation
   "In group-dms, the recipients will contain multiple entries"))

(defclass group-dm-channel (dm-channel)
  ((icon     :initarg :icon
	      :reader  icon?		; could be nil!
	      :type    (or null string))
   (owner-id :initarg :owner
	     :reader  owner?
	     :type    (or null string))))
