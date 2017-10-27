(in-package :lispcord.util)

(defun str-concat (&rest strings)
  (apply #'concatenate 'string strings))

