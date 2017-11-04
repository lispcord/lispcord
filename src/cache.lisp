(in-package :lispcord.cache)

(defstruct cache
  "A database object to store & access stateful discord information"
  (guilds (make-hash-table :test #'equal) :type hash-table)
  (users (make-hash-table :test #'equal) :type hash-table)
  (channels (make-hash-table :test #'equal) :type hash-table))

(defvar *cache* (make-cache)
  "The global cache object. It should be persistent, but should
it be shared across instances?")

(defun getc (type id)
  (case type
    (:guild (gethash id (cache-guilds *cache*)))
    (:user  (gethash id (cache-users *cache*)))
    (:channel (gethash id (cache-channels *cache*)))))

(defun setc (type id new-val)
  (case type
    (:guild (setf (gethash id (cache-guilds *cache*)) new-val))
    (:user  (setf (gethash id (cache-users *cache*)) new-val))
    (:channel (setf (gethash id (cache-users *cache*)) new-val))))

(defun cache-obj (type obj)
  (let ((cached? (getc type (gethash "id" obj))))
    (labels ((for-key (key val)
	       (unless (equalp val (gethash key cached?))
		 (setf (gethash key cached?) val))))
      (if cached?
	  (progn (maphash #'for-key obj) cached?)
	  (setc type (gethash "id" obj) obj)))))

(defun cache-guild (guild)
  (cache-obj :guild guild))

(defun cache-user (user)
  (cache-obj :user user))

(defun cache-channel (channel)
  (cache-obj :channel channel))

(defun print-hash (hash)
  (maphash (lambda (key val)
	     (format t "~&  ~30@a <- ~a~%"
		     key
		     (cond ((hash-table-p val) "<hashtable>")
			   ((stringp val) val)
			   ((vectorp val) "<vector>")
			   ((listp val) "<list>")
			   (T val))))
	   hash))
