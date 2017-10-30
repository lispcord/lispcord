;;;; The Guild, Member & Partial-Guild Objects

(defclass member ()
  ((user      :init-arg :user
	      :reader   user
	      :type     user)
   (nick      :init-arg :nick
	      :reader   nick? ; could be nil!
	      :type     (or null string))
   (roles     :init-arg :roles
	      :reader   roles
	      :type     (vector role))
   (joined-at :init-arg :joined-at
	      :reader   joined-at
	      :type     string)
   (deaf      :init-arg :deaf
	      :reader   deafp
	      :type     (or null t))
   (mute      :init-arg :mute
	      :reader   mutep
	      :type     (or null t))))

(defclass partial-guild ()
  ((id          :init-arg :id
                :reader   id
                :type     string)
   (unavailable :init-arg :unavailable
		:reader   available?
		:type     t))
  (:documentation
   "Discord sends this during the ready event for guilds that are offline or otherwise unavailable"))

(defclass guild ()
  ((id                            :init-arg :id
                                  :reader   id
                                  :type     string)
   (name                          :init-arg :name
	                          :reader   name
	                          :type     string)
   (icon                          :init-arg :icon
	                          :reader   icon
	                          :type     string)
   (splash                        :init-arg :splash
	                          :reader   splash
	                          :type     string)
   (owner-id                      :init-arg :owner
	                          :reader   owner
	                          :type     string)
   (region                        :init-arg :region
	                          :reader   region
	                          :type     string)
   (afk-channel-id                :init-arg :afk-channel
		                  :reader   afk-channel
		                  :type     :string)
   (afk-timeout                   :init-arg :afk-timeout
		                  :reader   afk-timeout
		                  :type     fixnum)
   (embed-enabled                 :init-arg :embed-enabled
		                  :reader   embed?
		                  :type     (or null t))
   (embed-channel-id              :init-arg :embed-channel
		                  :reader   embed-channel
		                  :type     string)
   (verification-level            :init-arg :verification-level
		                  :reader   verification-level
		                  :type     fixnum)
   (default-message-notifications :init-arg :default-notifications
                                  :reader   default-notifications
                                  :type     fixnum)
   (explicit-content-filter       :init-arg :content-filter
			          :reader   content-filter
			          :type     fixnum)
   (roles                         :init-arg :roles
	                          :reader   roles
	                          :type     (vector role))
   (emojis                        :init-arg :emojis
	                          :reader   emojis
	                          :type     (vector emoji))
   (features                      :init-arg :features
	                          :reader   features
	                          :type     (vector string))
   (mfa-level                     :init-arg :mfa-level
	                          :reader   mfa-level
	                          :type     fixnum)
   (application-id                :init-arg :app-id
		                  :reader   app-id? ; could be nil!
		                  :type     (or null string))
   (widget-enabled                :init-arg :widget
		                  :reader   widgetp
		                  :type     (or null t))
   (widget-channel-id             :init-arg :widget-channel
		                  :reader   widget-channel
		                  :type     string)
   ;; The following fields are only send during a GUILD_CREATE EVENT
   ;; All of them may or may not be nil, but the user shouldn't know
   ;; that, and it should be handled through the unavailable flag
   (joined-at                     :init-arg :joined-at
	                          :reader   joined-at
	                          :type     string)
   (large                         :init-arg :large
	                          :reader   largep
	                          :type     (or null t))
   ;;why is this not just available? o.o
   (unavailable                   :init-arg :unavailable
		                  :reader   unavailablep
		                  :type     (or null t))
   (member-count                  :init-arg :member-count
		                  :reader   member-count
		                  :type     fixnum)
   ;; there should be a VOICE_STATES field here, but idk if we need it
   (members                       :init-arg :members
	                          :reader   members
	                          :type     (vector member))
   (channels                      :init-arg :channels
	                          :reader   channels
	                          :type     (vector channel))
   ;; again, here should be a PRESENCES field but meh
   ))


