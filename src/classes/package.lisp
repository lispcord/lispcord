
;; [] define the core structures for this submodule
(defpackage :lispcord.classes.core
  (:use :cl :lispcord.util :jonathan)
  (:export #:from-json
	   #:!!))

;; [] define the structure and conversion methods for roles
(defpackage :lispcord.classes.role
  (:use :cl :lispcord.util :lispcord.classes.core :jonathan))

;; [] define the structure and conversion methods for roles
(defpackage :lispcord.classes.user
  (:use :cl :lispcord.util :lispcord.classes.core :jonathan))

;; [] define the structure and conversion methods for roles
(defpackage :lispcord.classes.channel
  (:use :cl :lispcord.util :lispcord.classes.core :jonathan))

;; [] define the structure and conversion methods for roles
(defpackage :lispcord.classes.guild
  (:use :cl :lispcord.util :lispcord.classes.core :jonathan))
