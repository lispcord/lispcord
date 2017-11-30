(in-package :lispcord.pipes)

;;; dude clean this the fuck up like heck
(defstruct (cargo (:constructor primitive-make-cargo))
  (tag :nil :type keyword)
  (origin "" :type string)
  (body nil :type t))

(defun make-cargo (tag body &optional origin)
  (primitive-make-cargo :tag tag :body body :origin origin))

(defun open-cargo (cargo)
  (let ((tag (cargo-tag cargo))
	(body (cargo-body cargo))
	(origin (cargo-origin cargo)))
    (values body tag origin)))


(defclass pipe ()
  ((handlers :initarg :handlers
	     :accessor handlers
	     :initform #()
	     :type array))
  (:documentation "The default pipe class. Used to handle events"))


(defun make-pipe ()
  (make-instance 'pipe))

(defun pipep (obj)
  (typep obj 'pipe))

(defun wipe-pipe (pipe)
  (setf (handlers pipe) #())
  pipe)

(defun pipe-along (pipe cargo)
  "Pipes the event along to the watchers of that pipe"
  (map nil (lambda (h) (funcall h cargo)) (handlers pipe)))

(defun watch (pipe fun)
  "subscribes to the event-feed of the pipe and returns the handler"
  (setf (handlers pipe) (cons fun (handlers pipe)))
  handler)

(defmacro watch-do (pipe lambda-list &body body)
  `(watch ,pipe (lambda ,lambda-list ,@body)))


(defun drop (pipe handler)
  "unsubscribes from the event-feed"
  (setf (handlers pipe) (remove handler (handlers pipe))))



(defun pmap (pipe fun)
  "For a pipe p with events e and a function f, returns a new pipe q whose elements are (f e)"
  (let* ((q (make-instance (class-of pipe)))
	 (h (watch-do pipe (val)
	      (pipe-along q (funcall fun val)))))
    (values q h)))


(defun pfilter (pipe pred)
  "For a pipe p with events k;e, returns a new pipe q whose elements satisfy predicate(k, e)"
  (let* ((q (make-instance (class-of pipe)))
	 (h (watch-do pipe (val)
	      (if (funcall pred val) (pipe-along q val)))))
    (values q h)))


(defun pjoin (&rest pipes)
  "For any amount of pipes, returns a new pipe which is the union of the base pipes"
  (let* ((q (make-instance (class-of (car pipes))))
	 (hs (mapf pipes (p)
		   (watch-do p (val)
		     (pipe-along q val)))))
    (values q hs)))



(defmacro with-cargo ((cargo tag body &optional origin) &body progn)
  `(multiple-value-bind ,(if origin
			     (list body tag origin)
			     (list body tag))
       (open-cargo ,cargo)
     ,@progn))

(defmacro watch-with-cargo ((pipe tag body &optional origin) &body progn)
  (let ((c (gensym)))
    `(watch-do ,pipe (,c)
       (with-cargo (,c ,tag ,body ,origin)
	 ,@progn))))

(defmacro watch-with-case ((pipe body &optional origin) &body progn)
  (let ((tag (gensym)))
    `(watch-with-cargo (,pipe ,tag ,body ,origin)
       (case ,tag
	 ,@progn))))

(defmacro watch-unwrapped ((pipe body &optional origin)
			   &body progn)
  (let ((_ (gensym "_")))
    `(watch-with-cargo (,pipe ,_ ,body ,origin)
       (declare (ignore ,_))
       ,@progn)))

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


