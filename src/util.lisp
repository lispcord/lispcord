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

(defmacro doit (&rest forms)
  `(let (it)
     ,@(mapcar (lambda (f) `(setf it ,f)) forms)))

(defmacro str-case (key-form &body clauses &aux (key (gensym)))
  `(let ((,key ,key-form))
     (cond ,@(mapcar (lambda (c)
		       (if (string= (string (car c)) "ELSE")
			   `(T ,@(cdr c))
			   `((string= ,key ,(car c)) ,@(cdr c))))
		     clauses))))
