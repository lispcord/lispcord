(defpackage :lispcord.util
  (:use :cl)
  (:export #:str-concat)
  (:export #:jparse)
  (:export #:jmake)
  (:export #:alist
	   #:aget
	   #:doit
	   #:str-case
	   #:split-string

	   #:set-debug-level
	   #:dprint))

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
  (:use :cl :lispcord.util :lispcord.ratelimits :lispcord.constants)
  (:export #:bot
	   #:primitive-make-bot
	   #:bot-token
	   #:bot-os
	   #:bot-lib
	   #:bot-version
	   #:bot-seq
	   #:bot-session-id
	   #:bot-conn
	   #:bot-heartbeat-thread
	   #:bot-callbacks
	   
	   #:bot-url
	   #:base-url
	   #:api-suffix
	   #:user-agent ;check if this can be private
	   #:headers    ;this too
	   #:get-rq
	   #:post-rq))

(defpackage :lispcord.gateway
  (:use :bordeaux-threads
	:cl
	:lispcord.util
	:lispcord.core
	:lispcord.constants)
  (:export :connect))

(defpackage :lispcord.http
  (:use :cl
	:lispcord.constants
	:lispcord.util
	:lispcord.core)
  (:export :send))

(defpackage :lispcord
  (:use :cl
	:lispcord.util
	:lispcord.constants
	:lispcord.gateway
	:lispcord.http
	:lispcord.core)
  (:export :make-bot
	   :connect-bot
	   :reply
	   #:with-bot-message))

(defpackage :lispcord.example
  ;; core, for the botstruct, can we hide away the bot struct from the user?
  (:use :cl :lispcord :lispcord.core)
  (:export :example))
