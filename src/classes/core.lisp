(in-package :lispcord.classes.core)

(defgeneric from-json (class-symbol obj)
  (:documentation "Converts a json object to the specified class"))

;;; The general structure of an object definition should be as follows:

;;; (defclass obj ()
;;;   ((field :initarg :field
;;;           :reader/accessor field
;;;           :type typespec)))
;;;
;;; (defmethod %to-json ((o obj))
;;;   (with-object
;;;      (write-key-value "field" (field  o))))
;;;
;;; (defmethod from-json ((c (eql :obj)) (table hash-table))
;;;   (with-table (table field "field")
;;;     (make-instance 'obj :field field)))

;;; deviations, of course, where appropriate
