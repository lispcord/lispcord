
(defpackage :lispcord.constants
  (:use :cl :lispcord.util)
  (:export #:+os+
	   #:+lib+
	   #:+base-url+
	   #:+api-suffix+
	   #:+gw-rate-limit+
	   #:+gw-rate-limit-connection+
	   #:+gw-rate-limit-game-status+))

(defpackage :lispcord.ratelimits
  (:use :cl :lispcord.util :lispcord.constants)
  (:export #:rl-parse
	   #:rl-buffer))

(defpackage :lispcord.core
  (:use :cl
	:lispcord.util
	:lispcord.ratelimits
	:lispcord.constants)
  (:export #:bot
	   #:primitive-make-bot
	   #:*client*

	   #:bot-token
	   #:bot-user
	   #:bot-version
	   #:bot-seq
	   #:bot-session-id
	   #:bot-afk-since
	   #:bot-event-handlers
	   #:bot-conn
	   #:bot-done
	   #:bot-heartbeat-thread

	   #:bot-url
	   #:base-url
	   #:api-suffix
	   #:discord-req
	   #:get-rq
	   #:post-rq))

(defpackage :lispcord.pipes
  (:use :cl :lispcord.util :lispcord.core)
  (:export #:make-event-table
	   #:add-event-handler
	   #:dispatch-event))

(defpackage :lispcord.gateway
  (:use :bordeaux-threads
	:cl
	:lispcord.util
	:lispcord.pipes
	:lispcord.core
	:lispcord.constants)
  (:import-from :lispcord.classes
		#:from-json
		#:%to-json
		#:cache
		#:getcache-id
		#:decache-id)
  (:export #:connect
	   #:disconnect))

(defpackage :lispcord.http
  (:use :cl
	:jonathan
	:lispcord.constants
	:lispcord.util
	:lispcord.core)
  (:import-from :lispcord.classes
		:cache
		:getcache-id
		:decache-id
		:from-json)
  (:export #:create
	   #:edit
	   #:erase
	   #:from-id
	   #:get-messages
	   #:erase-reaction
	   #:erase-messages
	   #:erase-overwrite
	   #:start-typing
	   #:get-pinned
	   #:pin
	   #:unpin
	   #:get-emojis
	   #:erase-emoji
	   #:get-channels
	   #:get-members
	   #:move-member
	   #:set-nick
	   #:erase-role
	   #:get-bans
	   #:ban
	   #:unban
	   #:get-roles
	   #:get-me
	   #:leave
	   #:create-dms))

(defpackage :lispcord
  (:use :cl
	:lispcord.util
	:lispcord.constants
	:lispcord.gateway
	:lispcord.http
	:lispcord.core
	:lispcord.pipes)
  (:export #:*client*
	   #:connect
	   #:disconnect
	   #:defbot

	   #:make-prefix
	   #:commandp
	   #:sanitize-content
	   #:remove-substring
	   #:remove-mention
	   	   
	   #:reply
	   #:me


	   #:add-event-handler

	   #:>message-create>
	   #:>message-update>
	   #:>message-delete>
	   #:>message-purge>
	   #:>reaction-add>
	   #:>reaction-remove>
	   #:>reaction-purge>
	   #:>pin-update>
	   #:>user-create>
	   #:>user-update>
	   #:>user-delete>
	   #:>guild-create>
	   #:>guild-update>
	   #:>guild-delete>
	   #:>presence-update>
	   #:>typing-start>
	   #:>status-ready>
	   #:>status-close>
	   #:>status-resumed>
	   #:>channel-create>
	   #:>channel-delete>
	   #:>channel-update>
	   #:>role-create>
	   #:>role-delete>
	   #:>role-update>
	   #:>member-add>
	   #:>member-remove>
	   #:>member-update>
	   #:>member-ban>
	   #:>member-unban>
	   #:>emoji-update>
	   #:>integrations-update>
	   	   
	   #:create
	   #:edit
	   #:erase
	   #:from-id
	   #:get-messages
	   #:erase-reaction
	   #:erase-messages
	   #:erase-overwrite
	   #:start-typing
	   #:get-pinned
	   #:pin
	   #:unpin
	   #:get-emojis
	   #:erase-emoji
	   #:get-channels
	   #:get-members
	   #:move-member
	   #:set-nick
	   #:erase-role
	   #:get-bans
	   #:ban
	   #:unban
	   #:get-roles
	   #:get-me
	   #:leave

	   #:set-log-level))


