;;;; The Guild, Member & Partial-Guild Objects

(in-package :lispcord.payloads)

(defclass gmember ()
  ((user      :initarg :user
	      :reader   user
	      :type     user)
   (nick      :initarg :nick
	      :reader   nick? ; could be nil!
	      :type     (or null string))
   (roles     :initarg :roles
	      :reader   roles
	      :type     (vector role))
   (joined-at :initarg :joined-at
	      :reader   joined-at
	      :type     string)
   (deaf      :initarg :deaf
	      :reader   deafp
	      :type     (or null t))
   (mute      :initarg :mute
	      :reader   mutep
	      :type     (or null t))))

(defclass partial-guild ()
  ((id          :initarg :id
                :reader   id
                :type     string)
   (unavailable :initarg :unavailable
		:reader   available?
		:type     t))
  (:documentation
   "Discord sends this during the ready event for guilds that are offline or otherwise unavailable"))

(defclass guild ()
  ((id                            :initarg :id
                                  :reader   id
                                  :type     string)
   (name                          :initarg :name
	                          :reader   name
	                          :type     string)
   (icon                          :initarg :icon
	                          :reader   icon
	                          :type     string)
   (splash                        :initarg :splash
	                          :reader   splash
	                          :type     string)
   (owner-id                      :initarg :owner
	                          :reader   owner
	                          :type     string)
   (region                        :initarg :region
	                          :reader   region
	                          :type     string)
   (afk-channel-id                :initarg :afk-channel
		                  :reader   afk-channel
		                  :type     :string)
   (afk-timeout                   :initarg :afk-timeout
		                  :reader   afk-timeout
		                  :type     fixnum)
   (embed-enabled                 :initarg :embed-enabled
		                  :reader   embed?
		                  :type     (or null t))
   (embed-channel-id              :initarg :embed-channel
		                  :reader   embed-channel
		                  :type     string)
   (verification-level            :initarg :verification-level
		                  :reader   verification-level
		                  :type     fixnum)
   (default-message-notifications :initarg :default-notifications
                                  :reader   default-notifications
                                  :type     fixnum)
   (explicit-content-filter       :initarg :content-filter
			          :reader   content-filter
			          :type     fixnum)
   (roles                         :initarg :roles
	                          :reader   roles
	                          :type     (vector role))
   (emojis                        :initarg :emojis
	                          :reader   emojis
	                          :type     (vector emoji))
   (features                      :initarg :features
	                          :reader   features
	                          :type     (vector string))
   (mfa-level                     :initarg :mfa-level
	                          :reader   mfa-level
	                          :type     fixnum)
   (application-id                :initarg :app-id
		                  :reader   app-id? ; could be nil!
		                  :type     (or null string))
   (widget-enabled                :initarg :widget
		                  :reader   widgetp
		                  :type     (or null t))
   (widget-channel-id             :initarg :widget-channel
		                  :reader   widget-channel
		                  :type     string)
   ;; The following fields are only send during a GUILD_CREATE EVENT
   ;; All of them may or may not be nil, but the user shouldn't know
   ;; that, and it should be handled through the unavailable flag
   (joined-at                     :initarg :joined-at
	                          :reader   joined-at
	                          :type     string)
   (large                         :initarg :large
	                          :reader   largep
	                          :type     (or null t))
   ;;why is this not just available? o.o
   (unavailable                   :initarg :unavailable
		                  :reader   unavailablep
		                  :type     (or null t))
   (gmember-count                  :initarg :member-count
		                  :reader   member-count
		                  :type     fixnum)
   ;; there should be a VOICE_STATES field here, but idk if we need it
   (gmembers                       :initarg :gmembers
	                          :reader   gmembers
	                          :type     (vector gmember))
   (channels                      :initarg :channels
	                          :reader   channels
	                          :type     (vector channel))
   ;; again, here should be a PRESENCES field but meh
   ))


