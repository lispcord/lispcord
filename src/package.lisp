(defpackage :cl-harmony.util
  (:use :cl)
  (:export #:str-concat))

(defpackage :cl-harmony
  (:use :cl :cl-harmony.util))
