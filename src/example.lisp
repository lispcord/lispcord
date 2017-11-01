(in-package :lispcord.example)

;; no idea how we should actually do this
;; but here's something for now xD

(defun example (token)
  (let ((bot (make-bot token)))
    (connect-bot bot)
    (with-bot-message (bot :message payload)
      (example-print (format nil "got message! ~a" payload)))
    bot))

(defun example-print (msg)
  (format t "[Example Bot] ~a~%" msg))
