(in-package :lispcord.pipes)

(defstruct (cargo (:constructor primitive-make-cargo))
  (tag nil :type (or null keyword))
  origin
  (body nil :type list))

(defun make-cargo (tag body &optional origin)
  (primitive-make-cargo :tag tag :body body :origin origin))

(defun open-cargo (cargo)
  (let ((tag (cargo-tag cargo))
	(body (cargo-body cargo))
	(origin (cargo-origin cargo)))
    (values body tag origin)))

(defun val (cargo)
  (cargo-body cargo))

(defun (setf val) (new-val cargo)
  (setf (cargo-body cargo) new-val))

(defstruct pipe
  (handlers #() :type array)
  upstream)


(defun wipe-pipe (pipe)
  (setf (pipe-handlers pipe) #())
  pipe)

(defun pipe-along (pipe cargo)
  "Pipes the event along to the watchers of that pipe"
  (loop :for h :across (pipe-handlers pipe)
     :do (funcall h cargo)))

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
	   (delete (car (pipe-upstream pipe))
		   (pipe-handlers e))))
   (cdr (pipe-upstream pipe))))



(defun pmap (pipe fun)
  "For a pipe p with events e and a function f, returns a new pipe q whose elements are (f e)"
  (let ((q (make-pipe)))
    (setf (pipe-upstream q)
	  (list
	   (watch-do pipe (cargo)
	     (pipe-along q (funcall fun cargo)))
	   pipe))
    q))


(defun pfilter (pipe pred)
  "For a pipe p with events e, returns a new pipe q whose elements satisfy predicate(e)"
  (let ((q (make-pipe)))
    (setf (pipe-upstream q)
	  (list 
	   (watch-do pipe (cargo)
	     (if (funcall pred cargo) (pipe-along q cargo)))
	   pipe))
    q))


(defun pjoin (&rest pipes)
  "For any amount of pipes, returns a new pipe which is the union of the base pipes"
  (let* ((q (make-pipe))
	 (l (lambda (e) (pipe-along q e))))
    (setf (pipe-upstream q) (cons l pipes))
    (mapcar (curry #'watch l) pipes)
    q))


(defun pfold (pipe fun initial-value)
  "For a pipe p and a function f, returns and updates a cargo-object produced from
applying f to the current value and any new event"
  (let* ((c (make-cargo :body initial-value))
	 (q (make-pipe)))
    (setf (pipe-upstream q)
	  (list
	   (watch-do pipe (cargo)
	     (setf (val c) (funcall fun (val c) cargo)))
	   pipe))
    (values c q)))



(defmacro with-cargo ((cargo tag body &optional origin) &body progn)
  `(multiple-value-bind ,(if origin
			     (list body tag origin)
			     (list body tag))
       (open-cargo ,cargo)
     ,@progn))


;;((:create msg) (dostuff msg))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun loop-case-body-clauses (cargo clauses)
    (loop :for e :in clauses :collect
       `(,(caar e)
	  (destructuring-bind ,(cdar e)
	      (cargo-body ,cargo)
	    ,@(cdr e))))))

(defmacro cargocase (cargo &body clauses)
  `(case (cargo-tag ,cargo)
     ,@(loop-case-body-clauses cargo clauses)))

(defmacro ccargocase (cargo &body clauses)
  `(ccase (cargo-tag ,cargo)
     ,@(loop-case-body-clauses cargo clauses)))

(defmacro ecargocase (cargo &body clauses)
  `(ecase (cargo-tag ,cargo)
     ,@(loop-case-body-clauses cargo clauses)))


(defun cargo-send (pipe tag body &optional origin)
  (pipe-along pipe (make-cargo tag body origin)))


(defun taggedp (tag cargo)
  (with-cargo (cargo ctag body)
    (declare (ignore body))
    (when (eq tag ctag) t)))

(defun from-origin-p (origin cargo)
  (with-cargo (cargo _ __ corigin)
    (declare (ignore _ __))
    (when (equal corigin origin))))


