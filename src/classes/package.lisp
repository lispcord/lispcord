
(defpackage :lispcord.classes.core
  (:use :cl :lispcord.util :jonathan)
  (:export #:from-json
	   #:!!))

(defpackage :lispcord.classes.user
  (:use :cl :lispcord.util :lispcord.classes.core :jonathan)
  (:export #:user))

(defpackage :lispcord.classes.emoji
  (:use :cl
	:lispcord.util
	:lispcord.classes.core
	:lispcord.classes.user
	:jonathan)
  (:export #:emoji))

(defpackage :lispcord.classes.channel
  (:use :cl :lispcord.util :lispcord.classes.core :jonathan)
  (:export #:role))


(defpackage :lispcord.classes.guild
  (:use :cl :lispcord.util :lispcord.classes.core :jonathan))


(defpackage :lispcord.classes.embed
  (:use :cl
	:lispcord.util
	:lispcord.classes.core
	:jonathan)
  (:export #:embed))

(defpackage :lispcord.classes.message
  (:use :cl
	:lispcord.util
	:lispcord.classes.core
	:lispcord.classes.user
	:lispcord.classes.emoji
	:lispcord.classes.guild
	:lispcord.classes.embed
	:jonathan))
