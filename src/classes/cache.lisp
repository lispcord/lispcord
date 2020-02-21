(in-package :lispcord.classes)

(defstruct (cache (:constructor primitive-make-cache))
  data
  (lock (bt:make-recursive-lock "LISPCORD.CLASSES cache")))

(defun make-cache (&optional (n 50))
  (primitive-make-cache :data (make-hash-table :test optimal-id-compare :size n)))

(defvar *users* (make-cache 200))

(defvar *guilds* (make-cache 10))

(defvar *channels* (make-cache 200))

(defvar *roles* (make-cache 100))

(defvar *emojis* (make-cache 200))

(defun resolve-cache (cache table key)
  (let ((id (parse-snowflake (gethash "id" table))))
    (bt:with-recursive-lock-held ((cache-lock cache))
      (let ((entity (gethash id (cache-data cache))))
        (v:debug :lispcord.cache "Cache-hit: ~20a :: ~a" id key)
        (if (and entity
                 (eq key (type-of entity)))
            (update table entity)
            (setf (gethash id (cache-data cache)) (from-json key table)))))))

(defun key-cache (key)
  (case key
    (user    *users*)
    (guild   *guilds*)
    (channel *channels*)
    (role    *roles*)
    (emoji   *emojis*)))

(defun cache (key table)
  (when table
    (let ((cache (key-cache key)))
      (resolve-cache cache table key))))


(defun getcache-id (id key)
  (let ((cache (key-cache key)))
    (gethash id (cache-data cache))))

(defun decache-id (id key)
  (let ((cache (key-cache key)))
    (remhash id (cache-data cache))))

(defun cache-update (id key table)
  (let ((cache (key-cache key)))
    (bt:with-recursive-lock-held ((cache-lock cache))
      (decache-id id key)
      (cache key table))))
