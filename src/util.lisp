(in-package :lispcord.util)

(defun str-concat (&rest strings)
  (apply #'concatenate 'string strings))

(defun jparse (payload)
  (jonathan:parse payload :as :alist))

(defun jmake (alist)
  (jonathan:to-json alist :from :alist))

(defun alist (car cdr &rest pairs)
  (if pairs
      (cons (cons car cdr)
	    (apply #'alist (car pairs) (cadr pairs) (cddr pairs)))
      (list (cons car cdr))))
