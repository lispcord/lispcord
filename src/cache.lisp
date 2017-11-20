(in-package :lispcord.cache)


(defvar *cache* (make-hash-table :test #'equal :size 100)
  "The global cache object. It should be persistent, but should
it be shared across instances?")


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


(defun cache (obj)
  (if (cachedp obj)
      (updatec (gethash "id" obj) obj)
      (setc (gethash "id" obj) obj)))

(watch-with-cargo (>ucache> tag user)
  (declare (ignore tag))
  (sethash "tag" user :user)
  (cache user))

(watch-with-cargo (>pcache> tag presence)
  (declare (ignore tag))
  (let ((id (gethash "id" (gethash "user" presence))))
    (sethash "status" (getc id) (gethash "status" presence))
    (sethash "game" (getc id) (gethash "game" presence))))


(watch-with-cargo (>ccache> tag channel)
  (declare (ignore tag))
  (sethash "tag" channel :channel)
  (if (gethash "recipients" channel)
      (sethash "recipients" channel 
	       (mapf (gethash "recipients" channel) (u)
		 (cargo-send >ucache> :update u)
		 (gethash "id" u))))
  (cache channel))

(watch-with-cargo (>rcache> tag payload)
  (declare (ignore tag))
  (with-table (payload role "role" gid "guild_id")
    (sethash (gethash "id" role) (gethash "roles" (getc gid))
	     role)))

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
