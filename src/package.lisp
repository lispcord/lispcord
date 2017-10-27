(defpackage :lispcord.util
  (:use :cl)
  (:export #:str-concat))

(defpackage :lispcord
  (:use :sb-ext :cl :lispcord.util))
