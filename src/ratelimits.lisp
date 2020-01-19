(in-package :lispcord.ratelimits)

(defparameter *ratelimitlimits*
  (make-hash-table :test #'equal)
  "Am i taking the piss on discord with those names? Yes")

(defparameter *ratelimitsrems*
  (make-hash-table :test #'equal)
  "The keys are the routes")

(defparameter *ratelimitsresets*
  (make-hash-table :test #'equal))

(defun reset-rate-limits ()
  (setf *ratelimitlimits* (make-hash-table :test #'equal)
  *ratelimitsrems* (make-hash-table :test #'equal)
  *ratelimitsresets* (make-hash-table :test #'equal)))

(defun clear (final)
  (setf (gethash final *ratelimitsrems*)
  (gethash final *ratelimitlimits*)))


(defun rl-buffer (endpoint)
  "Checks if the requested endpoint is hitting a ratelimit, and waits if so"
  ;;The api handles different routes differently, so we need to see which one
  ;; we get <.<
  (destructuring-bind (route &optional id? &rest rubbish)
      (split-sequence #\/ endpoint)
    (declare (ignore rubbish))
    (let* ((final (if (member route '("guilds" "channels")
            :test #'equal)
          (str-concat route "/" id?)
          route))
     (rl (gethash final *ratelimitsrems*))
     (cd (- (or (gethash final *ratelimitsresets*) 0)
      (since-unix-epoch))))
      (cond ((null rl) final)
      ((>= rl 1) (decf (gethash final *ratelimitsrems*)) final)
      ((minusp cd) (clear final) final)
      (t
       (v:warn :lispcord.ratelimits "Hitting ratelimit! Trying ~a again in: ~a" route cd)
       (sleep cd)
       (clear final)
       (rl-buffer endpoint))))))

(defun rl-parse (final headers)
  (flet ((geta (key alist) (cdr (assoc key alist :test #'eq)))
   (int (int) (parse-integer int)))
    (let ((limit? (geta :x-ratelimit-limit headers))
    (rem? (geta :x-ratelimit-remaining headers))
    (reset? (geta :x-ratelimit-reset headers)))
      (v:debug :lispcord.ratelimits "limit: ~a; Remaining: ~a; reset: ~a"
        limit? rem? reset?)
      (when (and limit? (not (= (int limit?) (gethash final *ratelimitlimits* 0))))
  (v:debug :lispcord.ratelimits "New ratelimit-limit ~a for ~a" limit? final)
  (setf (gethash final *ratelimitlimits*) (int limit?)))
      (when rem?
  (v:debug :lispcord.ratelimits "New ratelimit-remainder ~a for ~a" rem? final)
  (setf (gethash final *ratelimitsrems*) (int rem?)))
      (when (and reset? (not (= (int reset?) (gethash final *ratelimitsresets* 0))))
  (v:debug :lispcord.ratelimits "New ratelimit-reset ~a for ~a" reset? final)
  (setf (gethash final *ratelimitsresets*) (int reset?))))))
