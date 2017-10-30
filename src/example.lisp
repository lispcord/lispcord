(in-package :lispcord.example)

;; no idea how we should actually do this
;; but here's something for now xD

(defun example (token)
  (let ((bot (make-bot token)))
    (connect-bot bot)
        
;    (setf (gethash :message (bot-callbacks bot))
;           (lambda (payload) (example-print "hi")))))

; the above manual expansion works
; why does this say "payload is unbound" ?
    (with-bot-message (bot :message payload)
      (example-print (format t "got message")))))

(defun example-print (msg)
  (format t "[Example Bot] ~a~%" msg))
