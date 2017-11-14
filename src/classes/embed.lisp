(in-package :lispcord.classes.embed)

(defclass embed-footer ()
  ((text           :initarg :text
		   :type string)
   (icon           :initarg :icon
		   :type string)
   (icon-proxy-url :initarg :icon-proxy-url
		   :type string)))

(defmethod from-json ((c (eql :e-footer)) (table hash-table))
  (instance-from-table (table 'embed-footer)
    :text "text"
    :icon "icon_url"
    :icon-proxy-url "icon_proxy_url"))

(defmethod %to-json ((e embed-footer))
  (with-object
    (write-key-value "text" (!! e text))
    (write-key-value "icon_url" (!! e icon))
    (write-key-value "icon_proxy_url" (!! e icon-proxy-url))))

(defclass embed-generic ()
  ((url       :initarg :url
	      :type string)
   (proxy-url :initarg :proxy
	      :type string)
   (height    :initarg :height
	      :type fixnum)
   (width     :initarg :width
	      :type fixnum)))

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
    (write-key-value "url" (!! e url))
    (write-key-value "proxy_url" (!! e proxy-url))
    (write-key-value "height" (!! e height))
    (write-key-value "width" (!! e width))))



(defclass embed-video ()
  ((url    :initarg :url
	   :type string)
   (height :initarg :height
	   :type fixnum)
   (width  :initarg :width
	   :type fixnum)))

(defmethod from-json ((c (eql :e-video)) (table hash-table))
  (instance-from-table (table 'embed-video)
    :url "url"
    :height "height"
    :width "width"))

(defmethod %to-json ((e embed-video))
  (with-object
    (write-key-value "url" (!! e url))
    (write-key-value "height" (!! e height))
    (write-key-value "width" (!! e width))))


(defclass embed-provider ()
  ((name :initarg :name
	 :type string)
   (url  :initarg :url
	 :type string)))

(defmethod from-json ((c (eql :e-provider)) (table hash-table))
  (instance-from-table (table 'embed-provider)
    :name "name"
    :url "url"))

(defmethod %to-json ((e embed-provider))
  (with-object
    (write-key-value "name" (!! e name))
    (write-key-value "url" (!! e url))))


(defclass embed-author ()
  ((name           :initarg :name
		   :type string)
   (url            :initarg :url
		   :type string)
   (icon-url       :initarg :icon
		   :type string)
   (proxy-icon-url :initarg :proxy-icon-url
		   :type string)))

(defmethod from-json ((c (eql :e-author)) (table hash-table))
  (instance-from-table (table 'embed-author)
    :name "name"
    :url "url"
    :icon "icon_url"
    :proxy-icon-url "proxy_icon_url"))

(defmethod %to-json ((e embed-author))
  (with-object
    (write-key-value "name" (!! e name))
    (write-key-value "url" (!! e url))
    (write-key-value "icon_url" (!! e icon-url))
    (write-key-value "proxy_icon_url" (!! e proxy-icon-url))))


(defclass embed-field ()
  ((name   :initarg :name
	   :type string)
   (value  :initarg :value
	   :type string)
   (inline :initarg :inline
	   :type t)))

(defmethod from-json ((c (eql :e-field)) (table hash-table))
  (instance-from-table (table 'embed-field)
    :name "name"
    :value "value"
    :inline "inline"))

(defmethod %to-json ((e embed-field))
  (with-object
    (write-key-value "name" (!! e name))
    (write-key-value "value" (!! e value))
    (write-key-value "inline" (!! e inline))))

(defclass embed ()
  ((title       :initarg :title
		:type string)
   (type        :initarg :type
		:type string)
   (description :initarg :description
		:type string)
   (url         :initarg :url
		:type string)
   (timestamp   :initarg :timestamp
		:type string)
   (color       :initarg :color
		:type fixnum)
   (footer      :initarg :footer
		:type embed-footer)
   (image       :initarg :image
		:type embed-image)
   (thumbnail   :initarg :thumbnail
		:type embed-thumbnail)
   (video       :initarg :video
		:type embed-video)
   (provider    :initarg :provider
		:type embed-provider)
   (author      :initarg :author
		:type embed-author)
   (fields      :initarg :fields
		:type (vector embed-fields))))

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
    (write-key-value "title" (!! e title))
    (write-key-value "type" (!! e type))
    (write-key-value "description" (!! e description))
    (write-key-value "url" (!! e url))
    (write-key-value "timestamp" (!! e timestamp))
    (write-key-value "color" (!! e color))
    (write-key-value "footer" (!! e footer))
    (write-key-value "image" (!! e image))
    (write-key-value "thumbnail" (!! e thumbnail))
    (write-key-value "video" (!! e video))
    (write-key-value "provider" (!! e provider))
    (write-key-value "author" (!! e author))
    (write-key-value "fields" (!! e fields))))
