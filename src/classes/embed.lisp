(in-package :lispcord.classes)


(defclass embed-footer ()
  ((text           :initarg :text
		   :type string
		   :accessor text)
   (icon           :initarg :icon
		   :type string
		   :accessor icon)
   (icon-proxy-url :initarg :icon-proxy-url
		   :type string
		   :accessor icon-proxy)))

(defmethod from-json ((c (eql :e-footer)) (table hash-table))
  (instance-from-table (table 'embed-footer)
    :text "text"
    :icon "icon_url"
    :icon-proxy-url "icon_proxy_url"))

(defmethod %to-json ((e embed-footer))
  (with-object
    (write-key-value "text" (text e))
    (write-key-value "icon_url" (icon e))
    (write-key-value "icon_proxy_url" (icon-proxy e))))

(defclass embed-generic ()
  ((url       :initarg :url
	      :type string
	      :accessor url)
   (proxy-url :initarg :proxy
	      :type string
	      :accessor proxy-url)
   (height    :initarg :height
	      :type fixnum
	      :accessor height)
   (width     :initarg :width
	      :type fixnum
	      :accessor width)))

(deftype embed-image () 'embed-generic) 
(deftype embed-thumbnail () 'embed-generic)

(defmethod from-json ((c (eql :e-generic)) (table hash-table))
  (instance-from-table (table 'embed-generic)
    :url "url"
    :proxy "proxy_url"
    :height "height"
    :width "width"))

(defmethod %to-json ((e embed-generic))
  (with-object
    (write-key-value "url" (url e))
    (write-key-value "proxy_url" (proxy-url e))
    (write-key-value "height" (height e))
    (write-key-value "width" (width e))))



(defclass embed-video ()
  ((url    :initarg :url
	   :type string
	   :accessor url)
   (height :initarg :height
	   :type fixnum
	   :accessor height)
   (width  :initarg :width
	   :type fixnum
	   :accessor width)))

(defmethod from-json ((c (eql :e-video)) (table hash-table))
  (instance-from-table (table 'embed-video)
    :url "url"
    :height "height"
    :width "width"))

(defmethod %to-json ((e embed-video))
  (with-object
    (write-key-value "url" (url e))
    (write-key-value "height" (height e))
    (write-key-value "width" (width e))))


(defclass embed-provider ()
  ((name :initarg :name
	 :type string
	 :accessor name)
   (url  :initarg :url
	 :type string
	 :accessor url)))

(defmethod from-json ((c (eql :e-provider)) (table hash-table))
  (instance-from-table (table 'embed-provider)
    :name "name"
    :url "url"))

(defmethod %to-json ((e embed-provider))
  (with-object
    (write-key-value "name" (name e))
    (write-key-value "url" (url e))))


(defclass embed-author (sub-embed)
  ((name           :initarg :name
		   :type string
		   :accessor name)
   (url            :initarg :url
		   :type string
		   :accessor url)
   (icon-url       :initarg :icon
		   :type string
		   :accessor icon-url)
   (proxy-icon-url :initarg :proxy-icon-url
		   :type string
		   :accessor proxy-icon)))

(defmethod from-json ((c (eql :e-author)) (table hash-table))
  (instance-from-table (table 'embed-author)
    :name "name"
    :url "url"
    :icon "icon_url"
    :proxy-icon-url "proxy_icon_url"))

(defmethod %to-json ((e embed-author))
  (with-object
    (write-key-value "name" (name e))
    (write-key-value "url" (url e))
    (write-key-value "icon_url" (icon-url e))
    (write-key-value "proxy_icon_url" (proxy-icon e))))


(defclass embed-field ()
  ((name   :initarg :name
	   :type string
	   :accessor name)
   (value  :initarg :value
	   :type string
	   :accessor value)
   (inline :initarg :inline
	   :type t
	   :accessor inline)))

(defmethod from-json ((c (eql :e-field)) (table hash-table))
  (instance-from-table (table 'embed-field)
    :name "name"
    :value "value"
    :inline "inline"))

(defmethod %to-json ((e embed-field))
  (with-object
    (write-key-value "name" (name e))
    (write-key-value "value" (value e))
    (write-key-value "inline" (inline e))))

(defclass embed ()
  ((title       :initarg :title
		:type string
		:accessor title)
   (type        :initarg :type
		:type string
		:accessor type)
   (description :initarg :description
		:type string
		:accessor description)
   (url         :initarg :url
		:type string
		:accessor url)
   (timestamp   :initarg :timestamp
		:type string
		:accessor timestamp)
   (color       :initarg :color
		:type fixnum
		:accessor color)
   (footer      :initarg :footer
		:type embed-footer
		:accessor footer)
   (image       :initarg :image
		:type embed-image
		:accessor image)
   (thumbnail   :initarg :thumbnail
		:type embed-thumbnail
		:accessor thumbnail)
   (video       :initarg :video
		:type embed-video
		:accessor video)
   (provider    :initarg :provider
		:type embed-provider
		:accessor provider)
   (author      :initarg :author
		:type embed-author
		:accessor author)
   (fields      :initarg :fields
		:type (vector embed-field)
		:accessor fields)))

(defmethod from-json ((c (eql :embed)) (table hash-table))
  (instance-from-table (table 'embed)
    :title "title"
    :type "type"
    :description "description"
    :url "url"
    :timestamp "timestamp"
    :color "color"
    :footer (from-json :e-footer (gethash "footer" table))
    :image (from-json :e-generic (gethash "image" table))
    :thumbnail (from-json :e-generic (gethash "thumbnail" table))
    :video (from-json :e-video (gethash "video" table))
    :provider (from-json :e-provider (gethash "provider" table))
    :author (from-json :e-author (gethash "provider" table))
    :fields (map 'vector (curry #'from-json :e-field)
		 (gethash "fields" table))))

(defmethod %to-json ((e embed))
  (with-object
    (write-key-value "title" (title e))
    (write-key-value "type" (type e))
    (write-key-value "description" (description e))
    (write-key-value "url" (url e))
    (write-key-value "timestamp" (timestamp e))
    (write-key-value "color" (color e))
    (write-key-value "footer" (footer e))
    (write-key-value "image" (image e))
    (write-key-value "thumbnail" (thumbnail e))
    (write-key-value "video" (video e))
    (write-key-value "provider" (provider e))
    (write-key-value "author" (author e))
    (write-key-value "fields" (fields e))))
