(in-package :lispcord.http)

(defmethod from-id ((c (eql :channel)) id
		    &optional (bot *client*))
  (if (getcache-id id :channel)
      (getcache-id id :channel)
      (let ((flake (sf-to-string id)))
	(lc:cache :channel
		  (discord-req (str-concat "channels/" flake)
			       :bot bot)))))


;;application type nonsense?
(defclass new-msg ()
  ((content :initarg :content)
   (nonce   :initform (make-nonce))
   (tts     :initarg :tts)
   (file    :initarg :file)
   (embed   :initarg :embed)))

(defmethod %to-json ((m new-msg))
  (with-object
    (write-key-value "content" (slot-value m 'content))
    (write-key-value "nonce" (slot-value m 'nonce))
    (write-key-value "tts" (or (slot-value m 'tts) :false))
    (write-key-value "file" (or (slot-value m 'file) :false))
    (write-key-value "embed" (or (slot-value m 'embed) :false))))

(defun make-message (content &key tts file embed)
  (make-instance 'new-msg
		 :content content
		 :tts tts
		 :file file
		 :embed embed))

(defmethod create ((m new-msg) (c lc:channel)
		   &optional (bot *client*))
  (when (< 2000 (length (slot-value m 'content)))
    (error "Message size exceeds maximum discord message size!~%"))
  (let* ((nonce (slot-value m 'nonce))
	 (path (str-concat "channels/" (lc:id c) "/messages"))
	 (response (discord-req
		    path
		    :bot bot
		    :type :post
		    :content (to-json m)
		    :headers `(("Content-Type"
				. ,(if (slot-value m 'file)
				       "multipart/form-data"
				       "application/json")))))
	 (reply-nonce (gethash "nonce" response)))
    (if (equal reply-nonce nonce)
	(from-json :message response)
	(error "Could not send message, nonce failure of ~a ~a"
	       nonce reply-nonce))))

(defmethod create ((s string) (c lc:channel)
		   &optional (bot *client*))
  (create (make-message s) c bot))



