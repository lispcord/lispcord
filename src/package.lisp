

(defpackage :lispcord.pipes
  (:use :cl :lispcord.util)
  (:export #:make-pipe
	   #:pipep
	   #:pipe-along
	   #:watch
	   #:watch-do
	   #:drop
	   #:pmap
	   #:pfilter
	   #:pjoin

	   #:taggedp
	   #:from-origin-p

	   #:make-cargo
	   #:open-cargo
	   #:with-cargo
	   #:watch-with-cargo
	   #:watch-with-case
	   #:cargo-send))



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
	:lispcord.pipes
	:lispcord.ratelimits
	:lispcord.constants)
  (:export #:bot
	   #:primitive-make-bot
	   #:string-slot

	   #:token
	   #:user
	   #:version
	   #:seq
	   #:session-id
	   #:afk-since
	   #:conn
	   #:done
	   #:heartbeat-thread

	   #:>message>
	   #:>user>
	   #:>guild>
	   #:>status>
	   #:>channel>
	   
	   #:bot-url
	   #:base-url
	   #:api-suffix
	   #:discord-req
	   #:get-rq
	   #:post-rq))

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
  (:export #:send))

(defpackage :lispcord
  (:use :cl
	:lispcord.util
	:lispcord.constants
	:lispcord.gateway
	:lispcord.http
	:lispcord.core
	:lispcord.pipes)
  (:export #:make-bot
	   #:connect
	   #:disconnect
	   #:reply
	   #:with-handler
	   #:watch-with-case

	   #:>message>))


