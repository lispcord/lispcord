(in-package :lispcord.pipes)


(let ((counter 1))
  (defun generate-id ()
    (floor (+ (/ (get-universal-time) 100)
	      (* counter (get-universal-time))))))


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
    (values tag body origin)))


(defstruct handler
  (id (generate-id) :type fixnum)
  (fun (lambda (type cargo) (declare (ignore type cargo)))
       :type (function (cargo))))


(defclass pipe ()
  ((handlers :initarg :handlers
	     :accessor handlers
	     :initform nil
	     :type (cons handler)))
  (:documentation "The default pipe class. Used to handle events"))


(defun make-pipe ()
  (make-instance 'pipe))

(defun pipep (obj)
  (typep obj 'pipe))

(defun pipe-along (pipe cargo)
  "Pipes the event along to the watchers of that pipe"
  (map nil (lambda (h) (funcall (handler-fun h) cargo))
       (handlers pipe)))

(defun watch (pipe fun)
  "subscribes to the event-feed of the pipe and returns the handler-id"
  (let ((handler (make-handler :fun fun)))
    (setf (handlers pipe) (cons handler (handlers pipe)))
    handler))

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
  `(multiple-value-bind (,tag ,body ,origin) (open-cargo ,cargo)
     ,@progn))

(defun cargo-send (pipe tag body &optional origin)
  (pipe-along pipe (make-cargo tag body origin)))
