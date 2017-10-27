(defpackage :lispcord.util
  (:use :cl)
  (:export #:str-concat))

(defpackage :lispcord
  (:use :cl :lispcord.util))
