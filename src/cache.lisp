(in-package :lispcord.cache)


(defvar *cache* (make-hash-table :test #'equal :size 100)
  "The global cache object. It should be persistent, but should
it be shared across instances?")

;; we need to somehow parse the channels out of guild now
;; lift emojis/roles etc. into the global cache?


(defun wipe-cache ()
  (setf *cache* (make-hash-table :test #'equal :size 100)))



(defun getc (id)
  (gethash id *cache*))

;;maybe we just want to pass new-val here and extract the ID?
(defun setc (id new-val)
  (sethash id *cache* new-val))

(defun remc (id)
  (remhash id *cache*))

;;We don't want to clobber fields which were set before
(defmacro update-hash (place new-val)
  `(maphash (lambda (key val)
	      (sethash key ,place val))
	    ,new-val))

(defun updatec (id new-val)
  (update-hash (getc id) new-val))

(defun cachedp (obj)
  (and (hash-table-p obj)
       (getc (gethash "id" obj))))

(defun decache (obj)
  (remc (gethash "id" obj)))



(defun cache-user (user)
  (sethash "tag" user :user)
  (if (cachedp user)
      (updatec (gethash "id" user) user)
      (setc (gethash "id" user) user)))

(defun cache-presence (presence)
  (updatec (gethash "id" (gethash "user" presence))
	   (progn (remhash "user" presence) presence)))

(defun cache-channel (channel)
  (sethash "tag" channel :channel)
  (if (gethash "recipients" channel)
      (sethash "recipients"
	       channel
	       (mapcar (lambda (u)
			 (cache-user u)
			 (gethash "id" u))
		       (gethash "recipients" channel))))
  (if (cachedp channel)
      (updatec (gethash "id" channel) channel)
      (setc (gethash "id" channel) channel)))

;; Caching guilds is not that easy, especially since we
;; need to extract the channels and users and such to avoid
;; double caching

(defun find-id (id list)
  (find-if (lambda (p) (equal id (gethash "id" p))) list))

(defun gcache-obj (guild-id tag obj)
  (sethash "guild_id" obj guild-id)
  (sethash "tag" obj tag)
  (unless (equal (gethash "id" obj) guild-id)
    (if (cachedp obj)
	(updatec (gethash "id" obj) obj)
	(setc (gethash "id" obj) obj))))

(defun parse-member (member)
  (let ((user (gethash "user" member)))
    (if (cachedp user)
	(updatec (gethash "id" user) user)
	(setc (gethash "id" user) user))
    (sethash "user" member (gethash "id" user))
    member))

(defun cache-guild (guild)
  (let ((gid (gethash "id" guild)))
    (mapc (curry (gcache-obj gid :role))
	  (gethash "roles" guild))
    (sethash "role"
	     guild
	     (find-id gid (gethash "roles" guild)))
    (remhash "roles" guild)
    
    (mapc (curry (gcache-obj gid :channel))
	  (gethash "channels" guild))
    (sethash "channel"
	     guild
	     (find-id gid (gethash "channels" guild)))
    (remhash "channels" guild)
    
    (mapc (curry (gcache-obj gid :emoji))
	  (gethash "emojis" guild))
    ;; i don't think there's a "base" emoji so we'll skip this
    (remhash "emojis" guild)

    (sethash "members" guild
	  (mapcar #'parse-member (gethash "members" guild)))
    (sethash "tag" guild :guild)

    (if (cachedp guild)
	(updatec gid guild)
	(setc gid guild))))




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
