
(in-package :lispcord.classes)

(defgeneric to-plist (plist)
  (:documentation "Convert object recursively to flat data (for feeding to Jonathan)"))

(defmacro define-converters ((&rest methods) class-name &body converters)
  "Defines from-json, %to-json, and update methods for class-name.
methods is the list of methods to define (from-json %to-json).
from-json is implemented using update so they're both defined with
from-json

class-name is the name of the class to specialize methods on.

converters have the form (slot reader writer)

slot is the slot name. It will be read from a json field with the same name with dash switched for underscore

reader is the function to convert the data from json to a slot value.

writer is the function to convert the data from lisp slot value to json.
"
  (setf converters (mapcar 'ensure-list converters))
  `(progn
     ,@(when (cl:member 'from-json methods)
         (list (make-from-json class-name converters)
               (make-update class-name converters)))
     ,@(when (cl:member '%to-json methods)
         (list (make-%to-json class-name converters)
               (make-to-plist class-name converters)))
     (values)))

(defun make-from-json (class-name converters)
  (declare (ignore converters))
  (with-gensyms (obj)
    `(defmethod from-json ((class-symbol (eql ',class-name)) (table hash-table))
       (let ((,obj (make-instance class-symbol)))
         (update table ,obj)))))

(defun make-update (class-name converters)
  (with-gensyms (value)
    `(defmethod update ((table hash-table) (obj ,class-name))
       ;; Read slots from the table
       ,@(loop :for (slot reader writer) :in converters
            :for key := (slot->key slot)
            :if (not (eq reader :ignore))
            :collect `(let ((,value (gethash ,key table)))
                        (cond
                          ;; Slot is in the table. Update it
                          (,value (setf (slot-value obj ',slot) ,(if reader `(funcall ,reader ,value) value)))
                          ;; Slot is not in the table but is already bound. Leave it be
                          ((slot-boundp obj ',slot) nil)
                          ;; Slot is not in the table and is unbound. Initialize it
                          (t      (setf (slot-value obj ',slot) ,(if reader `(funcall ,reader nil)))))))
       ;; Process superclass
       (if (next-method-p)
           (call-next-method))
       obj)))

(defun make-%to-json (class-name converters)
  (declare (ignore converters))
  (with-gensyms (k v)
    `(defmethod %to-json ((obj ,class-name))
       (with-object
         (loop :for (,k ,v) :on (to-plist obj) :by #'cddr
            :do (write-key-value ,k ,v))))))

(defun make-to-plist (class-name converters)
  (with-gensyms ()
    `(defmethod to-plist ((obj ,class-name))
       (let ((plist nil))
         ,@(loop :for (slot reader writer) :in converters
              :if (not (eq writer :ignore))
              :collect `(push (if (slot-boundp obj ',slot)
                                  ,(if writer
                                       `(funcall ,writer
                                                 (slot-value obj ',slot))
                                       `(slot-value obj ',slot))
                                  ,(if writer
                                       `(funcall ,writer nil)
                                       nil))
                              plist)
              :if (not (eq writer :ignore))
              :collect `(push (slot->key ',slot) plist))
         (if (next-method-p)
             (append (call-next-method) plist)
             plist)))))

(defun slot->key (slot-name)
  "Slot name to json key"
  (map 'string (lambda (c)
                 (case c
                   (#\- #\_)
                   (t (char-downcase c))))
       (symbol-name slot-name)))

(defun key->slot (key)
  "Json key to slot name"
  (intern
   (map 'string (lambda (c)
                  (case c
                    (#\_ #\-)
                    (t (char-upcase c))))
        key)
   (find-package "LISPCORD.CLASSES")))

(defmacro define-alias (to from)
  ;; `to` is already bound and not to the same function this call
  ;; would have bound it previously
  (when (and (fboundp to)
             (not (eq (fdefinition to)
                      (fdefinition from))))
    (error "Defining the alias will overwrite existing function:~%~S" to))
  (when (not (fboundp from))
    (error "Defining an alias to a nonexisting function:~%~S" from))
  `(progn
     (export-pub ,to)
     (setf (fdefinition ',to) (fdefinition ',from))
     (setf (fdefinition '(setf ,to)) (fdefinition '(setf ,from)))))

(defmacro defclass* (name direct-superclasses direct-slots &rest options)
  "Defclass defaulting initarg and accessor name to slot name.
Also, exporting class name and all accessor from both lispcord.classes and lispcord.classes.pub"
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

;;;; Reader/writer function generators

(defun caching-reader (key)
  (lambda (value)
    (when value
      (cache key value))))

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
    (when table
      (from-json type table))))

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
