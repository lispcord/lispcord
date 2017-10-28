(defpackage :lispcord.util
  (:use :cl)
  (:export #:str-concat)
  (:export #:jparse)
  (:export #:jmake)
  (:export #:alist
	   #:doit
	   #:str-case))

(defpackage :lispcord.core
  (:use :cl :lispcord.util)
  (:export #:bot
	   #:bot-url
	   #:base-url
	   #:api-suffix
	   #:user-agent ;check if this can be private
	   #:headers    ;this too
	   #:get-rq
	   #:post-rq))

(defpackage :lispcord.gateway
  (:use :cl :lispcord.util :lispcord.core))

(defpackage :lispcord.http
  (:use :cl :lispcord.util :lispcord.core))


;;is it possible to remove the sb-ext? I'd like to keep this as portable as possible
(defpackage :lispcord
  (:use :sb-ext
	:cl
	:lispcord.util
	:lispcord.gateway
	:lispcord.http
	:lispcord.core))
