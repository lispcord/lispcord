
(defpackage :lispcord.classes.core
  (:use :cl :lispcord.util :jonathan)
  (:export #:from-json
	   #:!!))

(defpackage :lispcord.classes.user
  (:use :cl :lispcord.util :lispcord.classes.core :jonathan))

(defpackage :lispcord.classes.emoji
  (:use :cl
	:lispcord.util
	:lispcord.classes.core
	:lispcord.classes.user
	:jonathan))

(defpackage :lispcord.classes.channel
  (:use :cl :lispcord.util :lispcord.classes.core :jonathan))

(defpackage :lispcord.classes.guild
  (:use :cl :lispcord.util :lispcord.classes.core :jonathan))
