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

(define-converters (%to-json from-json) attachment
  (id 'parse-snowflake)
  filename size url proxy-url height width)

(defclass* reaction ()
  ((count :type fixnum)
   (me    :type boolean :accessor me-p)
   (emoji :type emoji)))

(define-converters (%to-json from-json) reaction
  count
  (me nil (defaulting-writer :false))
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

(define-converters (%to-json) partial-message
  (content)
  (nonce)
  (tts nil (defaulting-writer :false))
  (file nil (defaulting-writer :null))
  (embed nil (defaulting-writer :null)))

(defclass* message ()
  ((id :type snowflake)
   (channel-id :type snowflake)
   (guild-id :type (or null snowflake))
   (author :type user)
   (member :type (or null member) :accessor member%)
   (content :type string)
   (timestamp :type string)
   (edited-timestamp :type (or null string))
   (tts :type boolean :accessor tts-p)
   (mention-everyone :type boolean :accessor mention-everyone-p)
   (mentions :type (vector user))
   (mention-roles :type (vector role))
   (mention-channels :type (vector channel-mention))
   (attachments :type (vector attachment))
   (embeds :type (vector embed))
   (reactions :type (vector reaction))
   (nonce :type (or null fixnum string))
   (pinned :type boolean :accessor pinned-p)
   (webhook-id :type (or null snowflake))
   (type :type message-type)
   (activity :type (or null message-activity))
   (application :type (or null message-application))
   (message-reference :type (or null message-reference))
   (flags :type (or null fixnum))))

(defmethod member ((m message) &optional _)
  (member% m))

(defmethod (setf member) (new-value (m message) &optional _)
  (setf (member% m) new-value))

(define-converters (%to-json from-json) message
  (id 'parse-snowflake)
  (channel-id 'parse-snowflake)
  (guild-id '%maybe-sf)
  (author (subtable-reader 'user))
  (member (subtable-reader 'member))
  (edited-timestamp)
  (content)
  (timestamp)
  (edited-timestamp)
  (tts nil (defaulting-writer :false))
  (mention-everyone nil (defaulting-writer :false))
  (mentions (caching-vector-reader :user))
  (mention-roles (vector-reader 'parse-snowflake))
  (mention-channels (subtable-vector-reader 'channel-mention))
  (attachments (subtable-vector-reader 'attachment))
  (embeds (subtable-vector-reader 'embed))
  (reactions (subtable-vector-reader 'reaction))
  (nonce)
  (pinned nil (defaulting-writer :false))
  (webhook-id '%maybe-sf)
  (type)
  (activity (subtable-reader 'message-activity))
  (application (subtable-reader 'message-application))
  (message-reference (subtable-reader 'message-reference))
  (flags))

(defclass* message-activity ()
  ((type :type fixnum)
   (party-id :type (or null string))))

(define-converters (%to-json from-json) message-activity
  type party-id)

(defclass* message-application ()
  ((id :type snowflake)
   (cover-image :type (or null string))
   (description :type string)
   (icon :type (or null string))
   (name :type string)))

(define-converters (%to-json from-json) message-application
  (id 'parse-snowflake)
  cover-image description icon name)

(defclass* message-reference ()
  ((message-id :type (or null snowflake))
   (channel-id :type snowflake)
   (guild-id :type (or null snowflake))))

(define-converters (%to-json from-json) message-reference
  message-id channel-id guild-id)

(defclass* channel-mention ()
  ((id :type snowflake)
   (guild-id :type snowflake)
   (type :type fixnum)
   (name :type string)))

(define-converters (%to-json from-json) channel-mention
  (id 'parse-snowflake)
  (guild-id 'parse-snowflake)
  type name)
