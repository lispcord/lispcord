
(in-package :lispcord.classes)

(declaim (optimize (debug 3) (safety 3)))

;;;; Converters

(defvar *converters* (make-hash-table))

(defstruct (converter
            (:type list)
            (:constructor make-converter (slot &optional (reader 'identity) (writer 'identity))))
  slot reader writer)

(defun converters-for (obj)
  (let* ((class (class-of obj))
         (superclasses (c2mop:class-precedence-list class)))
    (mapcan (lambda (c)
              (when-let ((hash (gethash (class-name c) *converters* nil)))
                (hash-table-values hash)))
            superclasses)))

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
  (let ((converters (converters-for obj)))
    (with-object
      (loop :for (slot reader writer) :in converters
            :do (when (slot-boundp obj slot)
                  (write-key-value (slot->key slot)
                                   (funcall writer (slot-value obj slot))))))))

(defmethod from-json ((class-name symbol) (table hash-table))
  (let* ((obj (make-instance class-name))
         (slots (mapcar #'c2mop:slot-definition-name
                        (c2mop:class-slots (class-of obj)))))
    (update table obj)
    ;; We don't make a difference between optional and nullable values
    ;; I.e. if it's not there we pretend that it was null in json
    (let ((converters (converters-for obj)))
      (dolist (slot slots)
        (unless (slot-boundp obj slot)
          (let ((reader (converter-reader (find slot converters :key #'converter-slot))))
            (setf (slot-value obj slot)
                  (funcall reader nil))))))
    obj))

(defmethod update ((table hash-table) obj)
  (let ((converters (converters-for obj)))
    (loop :for (slot reader writer) :in converters
          :do (let ((key (slot->key slot)))
                (when (gethash key table nil)
                  (setf (slot-value obj slot)
                        (funcall reader (gethash key table)))))))
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
