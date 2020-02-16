(in-package :lispcord.classes)

(defclass overwrite ()
  ((id    :initarg :id
          :type snowflake
          :accessor id)
   (type  :initarg :type
          :type string
          :accessor type)
   (allow :initarg :allow
          :type permissions
          :accessor allow)
   (deny  :initarg :deny
          :type permissions
          :accessor deny)))

(defun make-overwrite (id &optional (allow 0) (deny 0) (type "role"))
  (make-instance 'overwrite :id id :type type
                 :allow (make-permissions allow)
                 :deny (make-permissions deny)))

(define-converters (overwrite)
  (id    'parse-snowflake)
  (type)
  (allow 'make-permissions)
  (deny  'make-permissions))

(defclass chnl nil nil)

(defclass partial-channel (chnl)
  ((name       :initarg :name       :accessor name)
   (position   :initarg :pos        :accessor position)
   (topic      :initarg :topic      :accessor topic)
   (nsfw       :initarg :nsfw       :accessor nsfw-p)
   (bitrate    :initarg :bitrate    :accessor bitrate)
   (user-limit :initarg :user-limit :accessor user-limit)
   (overwrites :initarg :overwrites :accessor overwrites)
   (parent-id  :initarg :parent     :accessor parent-id)
   (type       :initarg :type       :accessor type)))

(defun make-channel (&key name position topic nsfw
                       bitrate user-limit overwrites
                       parent-id type)
  (make-instance 'partial-channel
                 :name name
                 :pos position
                 :topic topic
                 :nsfw nsfw
                 :bitrate bitrate
                 :user-lim user-limit
                 :overwrites overwrites
                 :parent parent-id
                 :type type))

(define-converters (partial-channel)
  name pos top nsfw bit lim ovw parent type)

(defclass channel (chnl)
  ((id :initarg :id
       :type snowflake
       :accessor id)))

(define-converters (channel)
  (id 'parse-snowflake))

(defclass guild-channel (channel)
  ((guild-id      :initarg :guild-id
                  :type snowflake
                  :accessor guild-id)
   (name          :initarg :name
                  :type string
                  :accessor name)
   (position      :initarg :pos
                  :type fixnum
                  :accessor position)
   (permission-overwrites :initarg :permission-overwrites
                  :type (vector overwrite)
                  :accessor permission-overwrites)
   (parent-id     :initarg :parent-id
                  :type (or null snowflake)
                  :accessor parent-id)))

(define-converters (guild-channel)
  (guild-id 'parse-snowflake)
  (name)
  (position)
  (permission-overwrites (caching-vector-reader 'overwrite))
  (parent-id '%maybe-sf))

(defclass category (guild-channel)
  ((nsfw :initarg :nsfw
         :type boolean
         :accessor nsfw-p)))

(define-converters (category)
  nsfw)

(defmethod channels ((cat category))
  (let ((g (guild cat)))
    (remove-if-not (lambda (chan)
                     (eql (lc:id cat)
                          (lc:parent-id chan)))
                   (lc:channels g))))

(defclass text-channel (guild-channel)
  ((nsfw               :initarg :nsfw
                       :type boolean
                       :accessor nsfw-p)
   (topic              :initarg :topic
                       :type (or null string)
                       :accessor topic)
   (last-message-id    :initarg :last-message
                       :type (or null snowflake)
                       :accessor last-message)
   (last-pin-timestamp :initarg :last-pinned
                       :type string
                       :accessor last-pinned)))

(define-converters (text-channel)
  (nsfw)
  (topic)
  (last-message '%maybe-sf)
  (last-pinned))

(defclass voice-channel (guild-channel)
  ((bitrate    :initarg :bitrate
               :type fixnum
               :accessor bitrate)
   (user-limit :initarg :user-limit
               :type fixnum
               :accessor user-limit)))

(define-converters (voice-channel)
  (bitrate)
  (user-limit))

(defclass dm-channel (channel)
  ((last-message :initarg :last-message
                 :type (or null snowflake)
                 :accessor last-message)
   (recipients   :initarg :recipients
                 :type (vector user)
                 :accessor recipients)
   (last-pinned  :initarg :last-pinned
                 :type string
                 :accessor last-pinned)))

(define-converters (dm-channel)
  (recipients (caching-vector-reader 'user))
  (last-message '%maybe-sf)
  (last-pinned))

(defclass group-dm (dm-channel)
  ((name     :initarg :name
             :type string
             :accessor name)
   (icon     :initarg :icon
             :type (or null string)
             :accessor icon)
   (owner-id :initarg :owner
             :type snowflake
             :accessor owner-id)))

(define-converters (group-dm)
  (name)
  (icon)
  (owner-id 'parse-snowflake))

(defclass news-channel (guild-channel)
  ((nsfw         :initarg :nsfw
                 :type boolean
                 :accessor nsfw-p)
   (topic        :initarg :topic
                 :type (or null string)
                 :accessor topic)
   (last-message :initarg :last-message
                 :type (or null snowflake)
                 :accessor last-message)
   (last-pinned  :initarg :last-pinned
                 :type (or null string)
                 :accessor last-pinned)))

(define-converters (news-channel)
  (nsfw)
  (topic)
  (last-message '%maybe-sf)
  (last-pinned))

(defclass store-channel (guild-channel)
  ((nsfw         :initarg :nsfw
                 :type boolean
                 :accessor nsfw-p)))

(define-converters (news-channel)
  (nsfw))

(defmethod overwrite ((c channel) (_ (eql :everyone)))
  (find (guild-id c) (overwrites c) :key 'id))

(defmethod from-json ((c (eql 'channel)) (table hash-table))
  (from-json
   (case (gethash "type" table)
     (0 'text-channel)
     (1 'dm-channel)
     (2 'voice-channel)
     (3 'group-dm)
     (4 'category)
     (5 'news-channel)
     (6 'store-channel)
     (otherwise (v:error :lispcord.classes "Channel type ~A not recognised!~%This should only happen if discord creates a new channel type and lispcord wasn't updated yet~%Please file an issue at https://github.com/lispcord/lispcord/issues" (gethash "type" table))
      (return-from from-json nil)))
   table))
