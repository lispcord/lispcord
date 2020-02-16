
(in-package :lispcord.classes)

(declaim (optimize (debug 3) (safety 3)))

;;;; Converters

(defvar *converters* (make-hash-table))

(defstruct (converter
            (:type list)
            (:constructor make-converter (slot &optional (reader 'identity) (writer 'identity))))
  slot reader writer)

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
  (let ((obj (make-instance class-name)))
    (update table obj)))

(defmethod update ((table hash-table) obj)
  (maphash (lambda (type converters)
             (when (typep obj type)
               (loop :for (slot reader writer) :in (hash-table-values converters)
                     :do (when (gethash (slot->key slot) table nil)
                           (setf (slot-value obj slot)
                                 (funcall reader (gethash (slot->key slot) table)))))
               obj))
           *converters*)
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
