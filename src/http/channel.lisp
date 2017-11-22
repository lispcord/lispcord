(in-package :lispcord.http)

(defun get-channel (id &optional (bot nil))
  (from-json :channel (discord-req (str-concat "channels/" id)
				   :bot bot)))



