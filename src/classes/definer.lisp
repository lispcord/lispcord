
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

(defun converter (obj slot-name)
  (find slot-name (converters-for obj) :key #'converter-slot))

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

(defun key->slot (key)
  (let ((slot
         (find-symbol
          (map 'string (lambda (c)
                         (case c
                           (#\_ #\-)
                           (t (char-upcase c))))
               key)
          (find-package "LISPCORD.CLASSES"))))
    (or slot (cerror "IGNORE" "Unknown key from Discord API: ~S" key))))

;;; Fields not mentioned anywhere in Discord docs
(defvar *undocumented-field*
  '(discovery-splash rules-channel-id lazy public-updates-channel-id
    system-channel-flags hoisted-role sync-id))

(defun converter-key (converter)
  "Converter to json key"
  (slot->key (converter-slot converter)))

(defun read-slot (obj key value)
  (let ((slot (key->slot key)))
    (if-let ((converter (converter obj slot)))
      (let ((reader (converter-reader converter)))
        (unless (eq reader :ignore)
          (setf (slot-value obj slot)
                (funcall (converter-reader converter) value))))
      ;; Either Lispcord is missing the field, or (more probably) it's undocumented in Discord API Docs
      (v:trace :lispcord.classes "Field ~A in object of type ~A not recognised."
               key (type-of obj)))))

;;; Jonathan defines `write-key-value` as a macrolet in `with-object`
;;; Because of that we can't make `write-slot` a function...
(defmacro write-slot (obj slot)
  (once-only (obj slot)
    (with-gensyms (converter writer)
      `(if-let ((,converter (converter ,obj ,slot)))
         (let ((,writer (converter-writer ,converter)))
           (unless (eq ,writer :ignore)
             (write-key-value (slot->key ,slot)
                              (funcall ,writer (slot-value ,obj ,slot)))))
         ;; Either user adds new slots at runtime or we actually forgot to make a writer for it
         ;; Either way, report it asap
         (error "Object ~S~%Has field ~S~%But no writer defined for it" ,obj ,slot)))))

(defmethod %to-json (obj)
  (let ((slots (mapcar #'c2mop:slot-definition-name
                       (c2mop:class-slots (class-of obj)))))
    (with-object
      (dolist (slot slots)
        (when (slot-boundp obj slot)
          (write-slot obj slot))))))

(defmethod from-json ((class-name symbol) (table hash-table))
  (let ((obj (make-instance class-name)))
    (update table obj)
    ;; We don't make a difference between optional and nullable values
    ;; I.e. if it's not there we pretend that it was null in json
    (let ((converters (converters-for obj))
          (slots (mapcar #'c2mop:slot-definition-name
                         (c2mop:class-slots (class-of obj)))))
      (dolist (slot slots)
        (unless (slot-boundp obj slot)
          (when-let ((reader (converter-reader (or (find slot converters :key #'converter-slot)
                                                   (error "No converter for field ~A of class ~A" slot class-name)))))
            (unless (eq reader :ignore)
              (handler-case
                  (setf (slot-value obj slot)
                        (funcall reader nil))
                (type-error (e)
                  (error "Type error ~S~% on slot ~S~% of class ~S"
                         e slot class-name))))))))
    obj))

(defmethod update ((table hash-table) obj)
  (maphash (lambda (key value)
             ;; Fields starting with underscore are internal
             (unless (char= (elt key 0)
                            #\_)
               (read-slot obj key value)))
           table)
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
