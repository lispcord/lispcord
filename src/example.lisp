(in-package :lispcord.example)

;; no idea how we should actually do this
;; but here's something for now xD

(defvar *client*)


(defun start (token)
  (setf *client* (make-bot token))
  (connect *client*)
  (watch-do >message> (cargo)
    (with-cargo (cargo tag content)
      (if (eq tag :create)
	  (example-print (gethash "content" content))))))

(defun example-print (msg)
  (format t "Got message :D~% ~a~%" msg))
