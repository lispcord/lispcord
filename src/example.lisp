(in-package :lispcord.example)

;; no idea how we should actually do this
;; but here's something for now xD

(defvar *client*)

(defun start (token)
  (setf *client* (make-bot token))
  (connect *client*)
  (with-handler (msg bot :message)
    (example-print (format nil "got message! ~a" msg))))

(defun example-print (msg)
  (format t "[Example Bot] ~a~%" msg))
