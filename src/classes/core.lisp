(in-package :lispcord.classes)

(defgeneric from-json (class-symbol obj)
  (:documentation "Converts a json object to the specified class"))

(defmethod from-json (eh (n null))
  (declare (ignore eh n))
  nil)


(defgeneric update (data object)
  (:documentation "Updates the internal fields of the object"))

(defmacro from-table-update ((table var) &body clauses)
  (let ((key (gensym)))
    `(maphash (lambda (,key ,var)
		(case ,key
		  ,@ (loop :for (k a op) :in clauses :collect
			`(,k (setf ,a ,op)))))
	      ,table)))



;;; When working on a class, please comment on the relevant github issue
;;; at https://github.com/MegaLoler/lispcord/issues that you are working
;;; on it :)


;;; When deciding where a class should go, i guess it's best to follow
;;; the structure of the Discord API documentation
;;; If, however, a file is getting very long, or you have other good
;;; reasons to generate a new file, feel free to create it.
;;; Please remember to add it and the package to the lispcord.asd and
;;; src/classes/package.lisp file. Thank you <3



;;; The general structure of an object definition should be as follows:

;;; (defclass obj ()
;;;   ((field :initarg :field
;;;           :type typespec)))
;;;
;;; (defmethod %to-json ((o obj))
;;;   (with-object
;;;      (write-key-value "field" (field  o))))
;;;
;;; (defmethod from-json ((c (eql :obj)) (table hash-table))
;;;   (with-table (table field "field")
;;;     (make-instance 'obj :field field)))

;;; In the case where the api specifies fields that are not relevant
;;; to the user (like the _trace fields), or where defining a from- or
;;; to-json method makes no sense (because we only retrieve or send the
;;; object, lets say), feel free to omit those, alongside all other
;;; sensible deviations from the form :)
