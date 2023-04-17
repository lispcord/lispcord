
(defpackage :lispcord.constants
  (:use #:cl #:lispcord.util)
  (:export #:+os+
           #:+lib+
           #:+base-url+
           #:+api-suffix+
           #:+gw-rate-limit+
           #:+gw-rate-limit-connection+
           #:+gw-rate-limit-game-status+))

(defpackage :lispcord.ratelimits
  (:use #:cl #:lispcord.util #:lispcord.constants)
  (:local-nicknames (#:v #:verbose))
  (:export #:rl-parse
           #:rl-buffer))

(defpackage :lispcord.core
  (:use #:cl
        #:lispcord.util
        #:lispcord.ratelimits
        #:lispcord.constants)
  (:local-nicknames (#:v #:verbose))
  (:export #:bot
           #:%make-bot
           #:*client*

           #:bot-token
           #:bot-user
           #:bot-version
           #:bot-seq
           #:bot-session-id
           #:bot-afk-since
           #:bot-event-handlers
           #:bot-conn
           #:bot-running
           #:bot-heartbeat-ack
           #:bot-heartbeat-thread
           #:bot-auth-as-bot

           #:bot-auth
           #:bot-url
           #:base-url
           #:api-suffix
           #:discord-req
           #:get-rq
           #:post-rq))

(defpackage :lispcord.pipes
  (:use #:cl #:lispcord.util #:lispcord.core)
  (:export #:make-event-table
           #:add-event-handler
           #:dispatch-event))

(defpackage :lispcord.gateway
  (:use #:cl
        #:alexandria
        #:lispcord.util
        #:lispcord.pipes
        #:lispcord.core
        #:lispcord.constants)
  (:import-from #:lispcord.classes
                #:from-json
                #:%to-json
                #:cache
                #:getcache-id
                #:decache-id)
  (:local-nicknames (#:v #:verbose))
  (:export #:connect
           #:disconnect))

(defpackage :lispcord.http
  (:use #:cl
        #:alexandria
        #:jonathan
        #:lispcord.constants
        #:lispcord.util
        #:lispcord.core)
  (:import-from #:lispcord.classes
                #:cache
                #:getcache-id
                #:decache-id
                #:from-json
                #:botp)
  (:local-nicknames (#:v #:verbose))
  (:export #:botp
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
           #:leave
           #:create-dms))

(defpackage :lispcord
  (:use #:cl
        #:lispcord.util
        #:lispcord.constants
        #:lispcord.gateway
        #:lispcord.http
        #:lispcord.core
        #:lispcord.pipes)
  (:import-from #:lispcord.classes
                #:getcache-id
                #:botp)
  (:local-nicknames (#:v #:verbose))
  (:export #:*client*
           #:connect
           #:disconnect
           #:defbot
           #:make-bot
           #:botp

           #:make-prefix
           #:commandp
           #:sanitize-content
           #:remove-substring
           #:remove-mention
           #:mention
           #:demention
           
           #:reply
           #:me

           #:add-event-handler

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
           #:leave))


