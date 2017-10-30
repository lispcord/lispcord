;;; The Channel & overwrite objects

(defclass overwrites ()
  ((id    :init-arg :id
          :reader   user-id
          :type     string)
   (type  :init-arg :type
	  :reader   type
	  :type     string)
   (allow :init-arg :allow
	  :reader   allow
	  :type     fixnum)
   (deny  :init-arg :deny
	  :reader   deny
	  :type     fixnum))
  (:documentation "Sets the channel specific permissions for a user"))

(defclass channel ()
  ((id   :init-arg :id
         :reader   id
         :type     string)
   (type :init-arg :type
	 :reader   type
	 :type     (integer 0 4)))
  (:documentation "The basic channel object"))


(defclass guild-channel (channel)
  ((guild-id              :init-arg :guild-id
	                  :reader   guild-id?     ; could be nil!
	                  :type     (or null string))
   (position              :init-arg :position
	                  :reader   position?     ; could be nil!
	                  :type     (or null fixnum))
   (permission-overwrites :init-arg :permission-overwrites
			  :reader   permissions?  ; could be nil!
			  :type     (vector overwrites))
   (name                  :init-arg :name
	                  :reader   name?         ; could be nil!
	                  :type     (or null string))
   (parent-id             :init-arg :parent
			  :reader   parent?       ;could be nil!
			  :type     (or null string)))
  (:documentation "Guild channels are bound to a specific server"))

(defclass text-channel (guild-channel)
  ((topic           :init-arg :topic
	            :reader   topic?        ; could be nil!
	            :type     (or null string))
   (nsfw            :init-arg :nsfw
		    ; could be nil, but it's a bool anyways
	            :reader   nsfw?  
	            :type     (or null t))
   (last-message-id :init-arg :last-message
		    :reader   last-message? ; could be nil!
		    :type     (or null string)))
  (:documentation "Note that DM-channels are handled seperately"))

(defclass voice-channel (guild-channel)
  (bitrate    :init-arg :bitrate
	      :reader   bitrate?    ; could be nil!
	      :type     (or null fixnum))
  (user-limit :init-arg :user-limit
	      :reader   user-limit? ; could be nil!
	      :type     (or null fixnum)))

(defclass category (guild-channel)
  (nsfw :init-arg :nsfw
	:reader   nsfw? ; could be nil, but is a bool anyways
	:type     (or null t)))

(defclass dm-channel (channel)
  ((last-message-id :init-arg :last-message
		    :reader   last-message? ; could be nil!
		    :type     (or null string))
   (recipients      :init-arg :recipients
	            :reader   recipients?
	            :type     (or null (vector user))))
  (:documentation
   "In group-dms, the recipients will contain multiple entries"))

(defclass group-dm-channel (dm-channel)
  (icon     :init-arg :icon
	    :reader   icon? ; could be nil!
	    :type     (or null string))
  (owner-id :init-arg :owner
	    :reader   owner?
	    :type     (or null string)))
