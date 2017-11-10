
;; [] define the core structures for this submodule
(defpackage :lispcord.classes.core
  (:use :cl :lispcord.util))

;; [] define the structure and conversion methods for roles
(defpackage :lispcord.classes.role
  (:use :cl :lispcord.util :lispcord.classes.core))

;; [] define the structure and conversion methods for roles
(defpackage :lispcord.classes.user
  (:use :cl :lispcord.util :lispcord.classes.core))

;; [] define the structure and conversion methods for roles
(defpackage :lispcord.classes.channel
  (:use :cl :lispcord.util :lispcord.classes.core))

;; [] define the structure and conversion methods for roles
(defpackage :lispcord.classes.guild
  (:use :cl :lispcord.util :lispcord.classes.core))
