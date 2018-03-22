(in-package :example-bot)

(defparameter *commands* (make-hash-table :test #'equal))

(defun split (string &aux (last 0))
  "Splits a string at whitespace"
  (append (loop :for e :across string
	     :counting e :into c
	     :if (member e '(#\space #\newline #\tab))
	     :collect (prog1 (subseq string last (1- c))
			(setf last c)))
	  (list (subseq string last))))

(defun dispatch-commands (message)
  "processes the command and looks it up in *commands*"
  (let* ((no-mention (string-trim " " (remove-mention (me) (lc:content message)))))
    (destructuring-bind (cmd &rest args) (split no-mention)
      (apply (gethash (string-downcase cmd) *commands*) message args))))

(defmacro defcommand (command args &body body)
  "Constructs and registers a bot-command in *commands*"
  `(setf (gethash ,(string-downcase (string command)) *commands*)
	 (lambda ,args ,@body)))

(defcommand ping (msg)
  (reply msg "Pong!"))

(defcommand say (msg &rest words)
  (reply msg (format nil "~{~a~^ ~}" words)))

(defcommand bye (msg)
  (declare (ignore msg))
  (disconnect *client*))
