
(in-package :lispcord.classes)

(declaim (optimize (debug 3) (safety 3)))

;;;; Converters

(defvar *converters* (make-hash-table))

(defstruct (converter
            (:type list)
            (:constructor make-converter (slot &optional (reader 'identity) (writer 'identity))))
  slot reader writer)

(defmacro defclass* (name direct-superclasses direct-slots &rest options)
  "Defclass defaulting initarg and accessor name to slot name"
  (let ((accessor-names nil))
    (flet ((default-slot-options (direct-slot)
             (destructuring-bind (name &rest keys &key
                                                    (initarg (intern (symbol-name name) :keyword))
                                                    (accessor name)
                                  &allow-other-keys)
                 direct-slot
               (push accessor accessor-names)
               `(,name :initarg ,initarg
                       :accessor ,accessor
                       ,@(remove-from-plist keys :initarg :accessor)))))
      `(progn
         (defclass ,name ,direct-superclasses
           ,(mapcar #'default-slot-options direct-slots)
           ,@options)
         (lispcord.util:export-pub ,name)
         ,@(mapf (reverse accessor-names) (accessor-name)
             `(lispcord.util:export-pub ,accessor-name))))))

(defmacro define-converters ((class) &body converters)
  "converters have the form (slot reader writer)

slot is the slot name. It will be read from a json field with the same name with dash switched for underscore

reader is the function to convert the data from json to a slot value.

writer is the function to convert the data from lisp slot value to json.
"
  (with-gensyms (hash)
    `(let ((,hash (make-hash-table)))
       ,@(mapf converters (converter)
           (let ((converter (ensure-list converter)))
             `(setf (gethash ',(first converter) ,hash)
                    (make-converter ',(first converter) ,@(rest converter)))))
       (setf (gethash ',class *converters*) ,hash))))

(defun slot->key (slot-name)
  "Slot name to json key"
  (map 'string (lambda (c)
                 (case c
                   (#\- #\_)
                   (t (char-downcase c))))
       (symbol-name slot-name)))

(defun converter-key (converter)
  "Converter to json key"
  (slot->key (converter-slot converter)))

(defmethod %to-json (obj)
  (with-object
    (maphash (lambda (type converters)
               (when (typep obj type)
                 (loop :for (slot reader writer) :in (hash-table-values converters)
                       :do (when (slot-boundp obj slot)
                             (write-key-value (slot->key slot)
                                              (funcall writer (slot-value obj slot)))))))
             *converters*)))

(defmethod from-json ((class-name symbol) (table hash-table))
  (let* ((obj (make-instance class-name))
         (slots (mapcar #'c2mop:slot-definition-name
                        (c2mop:class-slots obj))))
    (update table obj)
    ;; We don't make a difference between optional and nullable values
    ;; I.e. if it's not there we pretend that it was null in json
    (dolist (slot slots)
      (unless (slot-boundp obj slot)
        (let ((reader (converter-reader (gethash slot (gethash class-name *converters*)))))
          (setf (slot-value obj slot)
                (funcall reader nil)))))))

(defmethod update ((table hash-table) obj)
  (let ((table (copy-hash-table table)))
    (maphash (lambda (type converters)
               (when (typep obj type)
                 (loop :for (slot reader writer) :in (hash-table-values converters)
                       :do (let ((key (slot->key slot)))
                             (when (gethash key table nil)
                               (setf (slot-value obj slot)
                                     (funcall reader (gethash key table)))
                               (remhash key table))))
                 obj))
             *converters*)
    (when (not (zerop (hash-table-size table)))
      (v:warn :lispcord.classes "Remaining unprocessed json fields: ~S~%This should only happen if Discord modifies API and lispcord wasn't updated yet~%Please file an issue at https://github.com/lispcord/lispcord/issues")))
  obj)

;;;; Reader/writer function generators

(defun caching-reader (key)
  (lambda (value)
    (cache key value)))

(defun caching-vector-reader (key)
  (lambda (value)
    (if value
        (mapvec (lambda (e) (cache key e))
                value)
        (make-array 0 :element-type key))))

(defun cache-vector-id-reader (key)
  (lambda (value)
    (if value
        (mapvec (lambda (e) (getcache-id (parse-snowflake e) key))
                value)
        (make-array 0 :element-type key))))

(defun subtable-reader (type)
  (lambda (table)
    (from-json type table)))

(defun subtable-vector-reader (type)
  (lambda (value)
    (map 'vector (curry 'from-json type)
         value)))

(defun vector-reader (parse-item)
  (lambda (value)
    (mapvec parse-item value)))

(defun defaulting-writer (default)
  (lambda (value)
    (or value default)))
