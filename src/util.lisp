(in-package :cl-harmony.util)

(defun str-concat (&rest strings)
  (apply #'concatenate 'string strings))

