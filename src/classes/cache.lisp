(in-package :lispcord.classes)

(defun make-cache (&optional (n 50))
  (make-hash-table :test optimal-id-compare :size n))

(defvar *users* (make-cache 200))

(defvar *guilds* (make-cache 10))

(defvar *channels* (make-cache 100))

(defvar *roles* (make-cache))

(defvar *emojis* (make-cache))

(defun resolve-cache (cache table key)
  (let* ((id (gethash "id" table))
	 (entity (gethash id cache)))
    (if entity
	(update table entity)
	(setf (gethash id cache) (from-json key table)))))

(defun cache (key table)
  (unless table (return-from cache nil))
  (case key
    (:user    (resolve-cache *users*    table key))
    (:guild   (resolve-cache *guilds*   table key))
    (:channel (resolve-cache *channels* table key))
    (:role    (resolve-cache *roles*    table key))
    (:emoji   (resolve-cache *emojis*   table key))))


(defun getcache-id (id key)
  (case key
    (:user    (gethash id *users*))
    (:guild   (gethash id *guilds*))
    (:channel (gethash id *channels*))
    (:role    (gethash id *roles*))
    (:emoji   (gethash id *emojis*))))
