
(defpackage :lispcord.classes.core
  (:use :cl :lispcord.util :jonathan)
  (:export #:from-json
	   #:!!))

(defpackage :lispcord.classes.user
  (:use :cl :lispcord.util :lispcord.classes.core :jonathan)
  (:export #:user
	   #:presence

	   #:from-json
	   #:%to-json

	   #:make-game))

(defpackage :lispcord.classes.integration
  (:use :cl
	:lispcord.util
	:lispcord.classes.core
	:lispcord.classes.user
	:jonathan)
  (:export #:integration

	   #:from-json
	   #:%to-json))

(defpackage :lispcord.classes.emoji
  (:use :cl
	:lispcord.util
	:lispcord.classes.core
	:lispcord.classes.user
	:jonathan)
  (:export #:emoji

	   #:from-json
	   #:%to-json))

(defpackage :lispcord.classes.channel
  (:use :cl :lispcord.util :lispcord.classes.core :jonathan)
  (:export #:channel

	   #:from-json
	   #:%to-json))


(defpackage :lispcord.classes.guild
  (:use :cl
	:lispcord.util
	:lispcord.classes.core
	:lispcord.classes.user
	:jonathan)
  (:export #:role
	   #:guild
	   #:guild-member

	   #:from-json
	   #:%to-json))


(defpackage :lispcord.classes.embed
  (:use :cl
	:lispcord.util
	:lispcord.classes.core
	:jonathan)
  (:export #:embed

	   #:from-json
	   #:%to-json))

(defpackage :lispcord.classes.message
  (:use :cl
	:lispcord.util
	:lispcord.classes.core
	:lispcord.classes.user
	:lispcord.classes.emoji
	:lispcord.classes.guild
	:lispcord.classes.embed
	:jonathan)
  (:export #:attachement
	   #:reaction
	   #:message

	   #:from-json
	   #:%to-json))


(defpackage :lispcord.classes
  (:use :cl
	:lispcord.classes.core
	:lispcord.classes.user
	:lispcord.classes.integration
	:lispcord.classes.emoji
	:lispcord.classes.channel
	:lispcord.classes.guild
	:lispcord.classes.embed
	:lispcord.classes.message)
  
  (:export #:%to-json
	   #:from-json

	   #:!!

	   #:user
	   #:presence
	   #:integration
	   #:emoji
	   #:channel
	   #:role
	   #:guild
	   #:guild-member
	   #:embed
	   #:attachement
	   #:reaction
	   #:message))
