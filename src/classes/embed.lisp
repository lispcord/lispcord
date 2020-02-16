(in-package :lispcord.classes)


(defclass embed-footer ()
  ((text           :initarg :text
                   :type string
                   :accessor text)
   (icon-url       :initarg :icon-url
                   :type (or null string)
                   :accessor icon-url)
   (proxy-icon-url :initarg :proxy-icon-url
                   :type (or null string)
                   :accessor proxy-icon-url)))

(define-converters (embed-footer)
  text icon-url proxy-icon-url)

(defclass embed-generic ()
  ((url       :initarg :url
              :type (or null string)
              :accessor url)
   (proxy-url :initarg :proxy-url
              :type (or null string)
              :accessor proxy-url)
   (height    :initarg :height
              :type (or null fixnum)
              :accessor height)
   (width     :initarg :width
              :type (or null fixnum)
              :accessor width)))

(define-converters (embed-generic)
  url proxy-url height width)

(defun make-embed-generic (&key url)
  (make-instance 'embed-generic
                 :url url
                 :proxy-url nil
                 :height nil
                 :width nil))

(deftype embed-image () 'embed-generic)
(deftype embed-thumbnail () 'embed-generic)

(defclass embed-video ()
  ((url    :initarg :url
           :type (or null string)
           :accessor url)
   (height :initarg :height
           :type (or null fixnum)
           :accessor height)
   (width  :initarg :width
           :type (or null fixnum)
           :accessor width)))

(define-converters (embed-video)
  url height width)

(defclass embed-provider ()
  ((name :initarg :name
         :type (or null string)
         :accessor name)
   (url  :initarg :url
         :type (or null string)
         :accessor url)))

(define-converters (embed-video)
  name url)

(defclass embed-author ()
  ((name           :initarg :name
                   :type (or null string)
                   :accessor name)
   (url            :initarg :url
                   :type (or null string)
                   :accessor url)
   (icon-url       :initarg :icon-url
                   :type (or null string)
                   :accessor icon-url)
   (proxy-icon-url :initarg :proxy-icon-url
                   :type (or null string)
                   :accessor proxy-icon-url)))

(define-converters (embed-author)
  name url icon-url proxy-icon-url)

(defclass embed-field ()
  ((name   :initarg :name
           :type string
           :accessor name)
   (value  :initarg :value
           :type string
           :accessor value)
   (inline :initarg :inline
           :type boolean
           :accessor inline)))

(define-converters (embed-author)
  name value inline)

(defclass embed ()
  ((title       :initarg :title
                :type (or null string)
                :accessor title)
   (type        :initarg :type
                :type (or null string)
                :accessor type)
   (description :initarg :description
                :type (or null string)
                :accessor description)
   (url         :initarg :url
                :type (or null string)
                :accessor url)
   (timestamp   :initarg :timestamp
                :type (or null string)
                :accessor timestamp)
   (color       :initarg :color
                :type (or null fixnum)
                :accessor color)
   (footer      :initarg :footer
                :type (or null embed-footer)
                :accessor footer)
   (image       :initarg :image
                :type (or null embed-image)
                :accessor image)
   (thumbnail   :initarg :thumbnail
                :type (or null embed-thumbnail)
                :accessor thumbnail)
   (video       :initarg :video
                :type (or null embed-video)
                :accessor video)
   (provider    :initarg :provider
                :type (or null embed-provider)
                :accessor provider)
   (author      :initarg :author
                :type (or null embed-author)
                :accessor author)
   (fields      :initarg :fields
                :type (vector embed-field)
                :accessor fields)))

(defun make-embed (&key title type description url timestamp color footer image thumbnail video provider author fields)
  (make-instance 'embed
                 :title title
                 :type type
                 :description description
                 :url url
                 :timestamp timestamp
                 :color color
                 :footer footer
                 :image image
                 :thumbnail thumbnail
                 :video video
                 :provider provider
                 :author author
                 :fields fields))

(define-converters (embed)
  title type description url timestamp color
  (footer    (subtable-reader 'embed-footer))
  (image     (subtable-reader 'embed-image))
  (thumbnail (subtable-reader 'embed-thumbnail))
  (video     (subtable-reader 'embed-video))
  (provider  (subtable-reader 'embed-provider))
  (author    (subtable-reader 'embed-author))
  (fields    (subtable-vector-reader 'embed-field)))
