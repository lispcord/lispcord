
;;; Both packages also export all class definitions and all accessors

(defpackage :lispcord.classes
  (:use :cl
        :alexandria
        :jonathan
        :lispcord.util)
  (:shadow #:type
           #:position
           #:inline
           #:count
           #:member)
  
  (:export #:%to-json
           #:from-json

           #:getcache-id
           #:cache
           #:decache-id
           #:cache-update))

(defpackage lispcord.classes.pub
  (:use :lispcord.classes)
  (:nicknames :lc))
