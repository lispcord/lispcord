(in-package :lispcord.http)

; send a message!
(defun send (bot channel-id content)
  (post-rq (str-concat "channels/" channel-id "/messages") bot
	   (alist "content" content)))

