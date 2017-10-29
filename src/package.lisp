(defpackage :lispcord.util
  (:use :cl)
  (:export #:str-concat)
  (:export #:jparse)
  (:export #:jmake)
  (:export #:alist
	   #:aget
	   #:doit
	   #:str-case

	   #:set-debug-level
	   #:dprint))

(defpackage :lispcord.core
  (:use :cl :lispcord.util)
  (:export #:bot
	   #:primitive-make-bot
	   #:bot-token
	   #:bot-os
	   #:bot-lib
	   #:bot-version
	   #:bot-seq
	   #:bot-conn
	   #:bot-heartbeat-thread
	   
	   #:bot-url
	   #:base-url
	   #:api-suffix
	   #:user-agent ;check if this can be private
	   #:headers    ;this too
	   #:get-rq
	   #:post-rq))

(defpackage :lispcord.gateway
  (:use :bordeaux-threads :cl ::lispcord.util :lispcord.core))

(defpackage :lispcord.http
  (:use :cl :lispcord.util :lispcord.core)
  (:export :send))

(defpackage :lispcord
  (:use :cl
	:lispcord.util
	:lispcord.gateway
	:lispcord.http
	:lispcord.core))
