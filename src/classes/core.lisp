(in-package :lispcord.classes)

(defgeneric from-json (class-symbol table)
  (:documentation "Converts a json object to the specified class"))

(defmethod from-json (eh (n null))
  (declare (ignore eh n))
  nil)

(defgeneric update (data object)
  (:documentation "Updates the internal fields of the object"))

(declaim (cl:inline %maybe-sf))
(defun %maybe-sf (string)
  "Only parses the string if it's there :D"
  (when string (parse-snowflake string)))

(defmethod guild (c)
  "Returns the cached guild object corresponding to the guild-id of object"
  (getcache-id (guild-id c) :guild))

(defmethod owner (c)
  "Returns the cached user object corresponding to the owner-id of object"
  (getcache-id (owner-id c) :user))

(defmethod channel (c)
  "Returns the cached channel object corresponding to the channel-id of object"
  (getcache-id (channel-id c) :user))

(defmethod parent (c)
  "Returns the cached channel object corresponding to the parent-id of object"
  (getcache-id (parent-id c) :channel))

;;; When working on a class, please comment on the relevant github issue
;;; at https://github.com/lispcord/lispcord/issues that you are working
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
;;; (define-converters (obj)
;;;   (field identity :null))

;;; In the case where the api specifies fields that are not relevant
;;; to the user (like the _trace fields), or where defining a from- or
;;; to-json method makes no sense (because we only retrieve or send the
;;; object, lets say), feel free to omit those, alongside all other
;;; sensible deviations from the form :)
