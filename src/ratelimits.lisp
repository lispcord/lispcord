(in-package :lispcord.ratelimits)

(defparameter *ratelimitlimits*
  (make-hash-table :test #'equal)
  "Am i taking the piss on discord with those names? Yes")

(defparameter *ratelimitsrems*
  (make-hash-table :test #'equal)
  "The keys are the routes")

(defparameter *ratelimitsresets*
  (make-hash-table :test #'equal))

(defun clear (final)
  (let ((it (gethash final *ratelimitsresets*)))
    (if (<= it 1)
	(setf (gethash final *ratelimitsrems*)
	      (gethash final *ratelimitlimits*)))))


(defun rl-buffer (endpoint)
  "Checks if the requested endpoint is hitting a ratelimit, and waits if so"
  ;;The api handles different routes differently, so we need to see which one
  ;; we get <.<
  (destructuring-bind (route &optional id? &rest rubbish)
      (split-string endpoint #\/)
    (declare (ignore rubbish))
    (let* ((final (if (member route '("guilds" "channels"))
		      (str-concat route "/" id?)
		      route))
	   (rl (gethash final *ratelimitsrems*))
	   (cd (gethash final *ratelimitsresets*)))
      (cond ((null rl) final)
	    ((> rl 1) (decf (gethash final *ratelimitsrems*)) final)
	    (t
	     (dprint :warn "Hitting ratelimit! Trying again in: ~a~%" cd)
	     (sleep cd)
	     (clear final)
	     (rl-buffer endpoint)
	     final)))))

(defun rl-parse (final headers)
  (let ((limit? (gethash "x-ratelimit-limit" headers))
	(rem? (gethash "x-ratelimit-remaining" headers))
	(reset? (gethash "x-ratelimit-reset" headers)))
    (when limit?
      (dprint :debug "New ratelimit-limit ~a for ~a~%" limit? final)
      (setf (gethash final *ratelimitlimits*) limit?))
    (when rem?
      (dprint :debug "New ratelimit-remainder ~a for ~a~%" rem? final)
      (setf (gethash final *ratelimitsrems*) rem?))
    (when reset?
      (dprint :debug "New ratelimit-reset ~a for ~a~%" reset? final)
      (setf (gethash final *ratelimitsresets*) (/ reset? 1000)))))
