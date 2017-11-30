(in-package :lispcord.http)

(defun get-me (&optional (bot *client*))
  (user bot))

(defmethod from-id (id (c (eql :user)) &optional (bot *client*))
  (cache :user (discord-req (str-concat "users/" id)
			    :bot bot)))


(defmethod edit ((u lc:user) (user lc:user) &optional (bot *client*))
    (cache :user
	   (discord-req
	    (str-concat "users/" (lc:id user))
	    :bot bot
	    :type :patch
	    :content (jmake
		      (list (cons "username" (lc:name u))
			    (cons "avatar" (lc:avatar u)))))))
