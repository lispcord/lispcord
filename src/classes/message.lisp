(in-package :lispcord.classes)

(deftype message-type () `(integer 0 7))

(defclass attachment ()
  ((id        :initarg :id
              :type snowflake
              :accessor id)
   (filename  :initarg :file
              :type string
              :accessor file)
   (size      :initarg :size
              :type fixnum
              :accessor size)
   (url       :initarg :url
              :type string
              :accessor url)
   (proxy-url :initarg :proxy
              :type string
              :accessor proxy-url)
   (height    :initarg :height
              :type (or null fixnum)
              :accessor height)
   (width     :initarg :width
              :type (or null fixnum)
              :accessor width)))

(define-converters (attachment)
  (id 'parse-snowflake)
  filename size url proxy-url height width)

(defclass reaction ()
  ((count :initarg :count
          :type (or null fixnum)
          :accessor count)
   (me    :initarg :me
          :type boolean
          :accessor me-p)
   (emoji :initarg :emoji
          :type (or null emoji)
          :accessor emoji)))

(define-converters (reaction)
  count me
  (emoji (caching-reader 'emoji)))

(defclass partial-message ()
  ((content :initarg :content :accessor content)
   (nonce   :initform (make-nonce) :accessor nonce)
   (tts     :initarg :tts :accessor tts-p)
   (file    :initarg :file :accessor file)
   (embed   :initarg :embed :accessor embed)))

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

(defclass message ()
  ((id            :initarg :id
                  :type snowflake
                  :accessor id)
   (channel-id    :initarg :channel-id
                  :type snowflake
                  :accessor channel-id)
   (author        :initarg :author
                  :type (or webhook user)
                  :accessor author)
   (content       :initarg :content
                  :type string
                  :accessor content)
   (timestamp     :initarg :timestamp
                  :type string
                  :accessor timestamp)
   (edited-timestamp :initarg :edited-timestamp
                  :type (or null string)
                  :accessor edited-timestamp)
   (tts           :initarg :tts
                  :type t
                  :accessor tts-p)
   (mention-everyone :initarg :mention-everyone
                  :type t
                  :accessor mention-everyone)
   (mentions      :initarg :mentions
                  :type (vector user)
                  :accessor mentions)
   (mention-roles :initarg :mention-roles
                  :type (vector role)
                  :accessor mention-roles)
   (attachments   :initarg :attachments
                  :type (vector attachment)
                  :accessor attachments)
   (embeds        :initarg :embeds
                  :type (vector embed)
                  :accessor embeds)
   (reactions     :initarg :reactions
                  :type (vector reaction)
                  :accessor reactions)
   (nonce         :initarg :nonce
                  :type (or null snowflake)
                  :accessor nonce)
   (pinned        :initarg :pinned
                  :type t
                  :accessor pinnedp)
   (type          :initarg :type
                  :type message-type
                  :accessor type)))

(defun user-or-webhook (obj)
  (if (gethash "webhook_id" obj)
      (from-json 'webhook obj)
      (cache 'user obj)))

(defmethod nick-or-name ((u user) (m message))
  "Member u of the guild with message m"
  (let ((c (channel m)))
    (if (typep c 'guild-channel)
        (nick-or-name u (guild c))
        (name u))))

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
