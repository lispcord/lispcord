(in-package :lispcord.pipes)

(defstruct (cargo (:constructor primitive-make-cargo)
		  :conc-name)
  (val nil))

(defun make-cargo (body)
  (primitive-make-cargo :val body))

(defstruct pipe
  (handlers #() :type (simple-array function (*)))
  upstream)

(defvar *origin*)

(defun wipe-pipe (pipe)
  (setf (pipe-handlers pipe) #())
  pipe)

(defun pipe-along (pipe cargo)
  "Pipes the event along to the watchers of that pipe"
  (loop :for h :across (pipe-handlers pipe)
     :do (apply h cargo)))

(defun watch (fun pipe)
  "subscribes to the event-feed of the pipe"
  (setf (pipe-handlers pipe)
	(vec-extend fun (pipe-handlers pipe)))
  fun)

(defmacro watch-do (pipe lambda-list &body body)
  `(watch (lambda ,lambda-list ,@body) ,pipe))


(defun drop (pipe)
  "unsubscribes pipe from upstream"
  (mapcar
   (lambda (e)
     (setf (pipe-handlers e)
	   (remove (car (pipe-upstream pipe))
		   (pipe-handlers e))))
   (cdr (pipe-upstream pipe)))
  nil)


(defmacro defpipe (symbol &key for from where do reduce with)
  (when (and for (not from))
    (error "Missing :FROM-clause in DEFPIPE ~a" symbol))
  (when (and reduce (not with))
    (error "Missing :WITH-clause in DEFPIPE ~a" symbol))
  (when (and (not reduce) with)
    (error "Missing :REDUCE-clause in DEFPIPE ~a" symbol))
  (when (and (not for) (or from where do reduce))
    (error "Missing :FOR-clause in DEFPIPE ~a" symbol))
  (let* ((l (gensym "LAMBDA"))
	 (q (gensym "PIPE"))
	 (r (gensym "_REDUCE"))
	 (f (if (consp for) for (list for)))
	 (ignore-list (remove-if-not
		       (lambda (e) (char= #\_ (char (symbol-name e) 0)))
		       f))
	 (ff (if (consp from) from (list from))))
    `(progn
       (when (and (boundp ',symbol) (pipe-p ,symbol))
	 (drop ,symbol))
       (defparameter ,symbol
	 ,(if for
	      `(let* ((,q (make-pipe))
		      (,l (lambda ,f
			    (declare (ignore ,@ignore-list))
			    (let ((,(or reduce r) ,symbol))
			      ,(unless reduce `(declare (ignore ,r)))
			      (when ,(or where t)
				,(if reduce
				     `(setf ,symbol ,(or do for))
				     `(pipe-along ,q (list ,(or do for)))))))))
		 (setf (pipe-upstream ,q)
		       (cons ,l (list ,@ff)))
		 (mapcar (curry #'watch ,l)
			 (list ,@ff))
		 ,(if (and reduce with)
		      with
		      q))
	      `(make-pipe))))))


(defun cargo-send (pipe body &optional origin)
  (declare (type cons body))
  (let ((*origin* origin))
    (pipe-along pipe body)))


(defun from-origin-p (bot)
  (if *origin*
      (eq *origin* bot)))


