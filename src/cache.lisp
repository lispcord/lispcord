(in-package :lispcord.cache)


(defvar *cache* (make-hash-table :test #'equal :size 100)
  "The global cache object. It should be persistent, but should
it be shared across instances?")

(defstruct (cache :conc-name) 
  (guilds
   (make-hash-table :test optimal-snowflake-compare))
  (users
   (make-hash-table :test optimal-snowflake-compare))
  (channels
   (make-hash-table :test optimal-snowflake-compare))
  (roles
   (make-hash-table :test optimal-snowflake-compare))
  (emojis
   (make-hash-table :test optimal-snowflake-compare))
  (integrations
   (make-hash-table :test optimal-snowflake-compare)))

(defvar *cache* (make-cache))

;;; Setting up relevant subpipes for things that need to be cached

(defvar >gcache>
  (pfilter >guild> (lambda (c) (or (taggedp :create c)
				   (taggedp :update c)
				   (taggedp :emojis-update c)))))

(defvar >rcache>
  (pfilter >guild> (lambda (c) (or (taggedp :role-create c)
				   (taggedp :role-update c)))))

(defvar >mcache>
  (pfilter >guild> (lambda (c) (or (taggedp :member-add c)
				   (taggedp :member-update c)))))

(defvar >pcache> (pfilter >guild> (curry #'taggedp :presence)))

(defvar >ccache>
  (pfilter >channel> (lambda (c) (or (taggedp :create c)
				     (taggedp :update c)))))

(defvar >ucache> (pfilter >user> (curry #'taggedp :update)))

(defun wipe-cache () (setf *cache* (make-cache)))

(defmacro getcache (obj kind)
  `(gethash (id ,obj) (,kind *cache*)))

(defmacro setcache (obj kind)
  `(setf (gethash (id ,obj) (,kind *cache*)) obj))





(watch-with-cargo (>ucache> tag user)
  (declare (ignore tag))
  (setcache user users))

(watch-with-cargo (>pcache> tag presence)
  (declare (ignore tag))
  (setf (game (getcache (user presence))) (game presence))
  (setf (status (getcache (user presence))) (status presence)))


(watch-with-cargo (>ccache> tag channel)
  (declare (ignore tag))
  (when (typep channel lc:guild-channel)
    ())
  (setcache channel channels))

(watch-with-cargo (>rcache> tag role)
  (declare (ignore tag))
  (setcache payload))

(watch-with-cargo (>mcache> tag member)
  (declare (ignore tag))
  (let ((uid (gethash "id" (gethash "user" member)))
	(gid (gethash "guild_id" member)))
    (cargo-send >ucache> :update (gethash "user" member))
    (sethash "user" member uid)
    (sethash "members" (getc gid)
	     (cons member (gethash "members" (getc gid))))))


;; Caching guilds is not that easy, especially since we
;; need to extract the channels and users and such to avoid
;; double caching

(defun find-id (id list)
  (find-if (lambda (p) (equal id (gethash "id" p))) list))

(defun parse-member (member)
  (let ((user (gethash "user" member)))
    (if (cachedp user)
	(updatec (gethash "id" user) user)
	(setc (gethash "id" user) user))
    (sethash "user" member (gethash "id" user))
    member))

;; (defun cache-guild (guild)
;;   (with-table (guild gid "id"
;; 		     roles "roles"
;; 		     channels "channels"
;; 		     members "members")

;;     (sethash "roles" guild
;; 	     (let ((ht (make-hash-table :test #'equal
;; 					:size (length roles))))
;; 	       (mapf roles (r) (sethash (gethash "id" r) ht r))
;; 	       ht))
    
;;     (mapc (curry #'gcache-obj gid :channel)
;; 	  (gethash "channels" guild))
;;     (sethash "channel"
;; 	     guild
;; 	     (find-id gid (gethash "channels" guild)))
;;     (remhash "channels" guild)
    
;;     (mapc (curry #'gcache-obj gid :emoji)
;; 	  (gethash "emojis" guild))
;;     ;; i don't think there's a "base" emoji so we'll skip this
;;     (remhash "emojis" guild)

;;     (sethash "members" guild
;; 	  (mapcar #'parse-member (gethash "members" guild)))
;;     (sethash "tag" guild :guild)

;;     (if (cachedp guild)
;; 	(updatec gid guild)
;; 	(setc gid guild))))




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
