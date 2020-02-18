(in-package :lispcord.classes)

(deftype message-type () `(integer 0 7))

(defclass* attachment ()
  ((id        :type snowflake)
   (filename  :type string)
   (size      :type fixnum)
   (url       :type string)
   (proxy-url :type string)
   (height    :type (or null fixnum))
   (width     :type (or null fixnum))))

(define-converters (attachment)
  (id 'parse-snowflake)
  filename size url proxy-url height width)

(defclass* reaction ()
  ((count :type (or null fixnum))
   (me    :type boolean :accessor me-p)
   (emoji :type (or null emoji))))

(define-converters (reaction)
  count me
  (emoji (caching-reader 'emoji)))

(defclass* partial-message ()
  ((content)
   (nonce :initform (make-nonce))
   (tts :accessor tts-p)
   (file)
   (embed)))

(export-pub make-message)
(defun make-message (content &key tts file embed)
  (make-instance 'partial-message
                 :content content
                 :tts tts
                 :file file
                 :embed embed))

(define-converters (partial-message)
  (content)
  (nonce)
  (tts 'identity (defaulting-writer :false))
  (file 'identity (defaulting-writer :null))
  (embed 'identity (defaulting-writer :null)))

(defclass* message ()
  ((id :type snowflake)
   (channel-id :type snowflake)
   (author :type (or webhook user))
   (content :type string)
   (timestamp :type string)
   (edited-timestamp :type (or null string))
   (tts :type boolean :accessor tts-p)
   (mention-everyone :type boolean :accessor mention-everyone-p)
   (mentions :type (vector user))
   (mention-roles :type (vector role))
   (attachments :type (vector attachment))
   (embeds :type (vector embed))
   (reactions :type (vector reaction))
   (nonce :type (or null snowflake))
   (pinned :type boolean :accessor pinned-p)
   (type :type message-type)))

(define-converters (message)
  (id 'parse-snowflake)
  (channel-id 'parse-snowflake)
  (author 'user-or-webhook)
  (edited-timestamp)
  (mention-everyone)
  (mentions (caching-vector-reader 'user))
  (mention-roles (vector-reader 'parse-snowflake))
  (attachments (subtable-vector-reader 'attachment))
  (embeds (subtable-vector-reader 'embed))
  (reactions (subtable-vector-reader 'reaction))
  nonce pinned type tts timestamp content)
