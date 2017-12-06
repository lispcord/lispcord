(in-package :lispcord.pipes)

(defstruct (cargo (:constructor primitive-make-cargo)
		  :conc-name)
  (val nil))

(defun make-cargo (body)
  (primitive-make-cargo :val body))

(defstruct pipe
  (handlers #() :type array)
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
   (cdr (pipe-upstream pipe))))



(defun pmap (pipe fun)
  "For a pipe p with events e and a function f, returns a new pipe q whose elements are (f e)"
  (let ((q (make-pipe)))
    (setf (pipe-upstream q)
	  (list
	   (watch-do pipe (&rest cargo)
	     (pipe-along q (apply fun cargo)))
	   pipe))
    q))


(defun pfilter (pipe pred)
  "For a pipe p with events e, returns a new pipe q whose elements satisfy predicate(e)"
  (let ((q (make-pipe)))
    (setf (pipe-upstream q)
	  (list 
	   (watch-do pipe (&rest cargo)
	     (if (apply pred cargo) (pipe-along q cargo)))
	   pipe))
    q))


(defun pjoin (&rest pipes)
  "For any amount of pipes, returns a new pipe which is the union of the base pipes"
  (let* ((q (make-pipe))
	 (l (lambda (&rest e) (pipe-along q e))))
    (setf (pipe-upstream q) (cons l pipes))
    (mapcar (curry #'watch l) pipes)
    q))


(defun pfold (pipe fun initial-value)
  "For a pipe p and a function f, returns and updates a cargo-object produced from
applying f to the current value and any new event"
  (let* ((c (make-cargo initial-value))
	 (q (make-pipe)))
    (setf (pipe-upstream q)
	  (list
	   (watch-do pipe (cargo)
	     (setf (val c) (funcall fun (val c) cargo)))
	   pipe))
    (values c q)))


(defun cargo-send (pipe body &optional origin)
  (declare (type cons body))
  (let ((*origin* origin))
    (pipe-along pipe body)))


(defun from-origin-p (bot)
  (if *origin*
      (eq *origin* bot)))


