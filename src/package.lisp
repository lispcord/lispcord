(defpackage :lispcord.util
  (:use :cl)
  (:export #:str-concat)
  (:export #:jparse)
  (:export #:jmake)
  (:export #:alist))

(defpackage :lispcord
  (:use :sb-ext :cl :lispcord.util))
