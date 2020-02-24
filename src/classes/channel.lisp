(in-package :lispcord.classes)

(defclass* overwrite ()
  ((id    :type snowflake)
   (type  :type string)
   (allow :type permissions)
   (deny  :type permissions)))

(export-pub make-overwrite)
(defun make-overwrite (id &optional (allow 0) (deny 0) (type "role"))
  (make-instance 'overwrite :id id :type type
                 :allow (make-permissions allow)
                 :deny (make-permissions deny)))

(define-converters (%to-json from-json) overwrite
  (id    'parse-snowflake)
  (type)
  (allow 'make-permissions)
  (deny  'make-permissions))

(defclass* partial-channel ()
  ((name)
   (position)
   (topic)
   (nsfw)
   (bitrate)
   (user-limit)
   (permission-overwrites)
   (parent-id)
   (type)))

(export-pub make-channel)
(defun make-channel (&rest args &key name position topic nsfw
                       bitrate user-limit overwrites
                       parent-id type)
  (declare (ignore name position topic nsfw
                   bitrate user-limit overwrites
                   parent-id type))
  (apply 'make-instance 'partial-channel args))

(define-converters (%to-json) partial-channel
  name position topic bitrate user-limit permission-overwrites parent-id type
  (nsfw nil (defaulting-writer :false)))

(defclass* base-channel ()
  ((id :type snowflake)
   (type :type fixnum)))

(define-converters (%to-json from-json) base-channel
  (id 'parse-snowflake)
  type)

(defclass* guild-channel (base-channel)
  ((guild-id      :type (or null snowflake)) ;; It's not present in GUILD_CREATE events
   (name          :type string)
   (position      :type fixnum)
   (permission-overwrites :type (vector overwrite))
   (parent-id     :type (or null snowflake))
   (nsfw          :type boolean :accessor nsfw-p)))

(define-converters (%to-json from-json) guild-channel
  (guild-id '%maybe-sf)
  (name)
  (position)
  (permission-overwrites (subtable-vector-reader 'overwrite))
  (parent-id '%maybe-sf)
  (nsfw nil (defaulting-writer :false)))

(defclass* category (guild-channel) ())

(define-converters (%to-json from-json) category)

(defclass* text-channel (guild-channel)
  ((rate-limit-per-user :type fixnum)
   (topic               :type (or null string))
   (last-message-id     :type (or null snowflake))
   (last-pin-timestamp  :type (or null string))))

(define-converters (%to-json from-json) text-channel
  (rate-limit-per-user)
  (topic)
  (last-message-id '%maybe-sf)
  (last-pin-timestamp))

(defclass* voice-channel (guild-channel)
  ((topic      :type (or null string))
   (bitrate    :type fixnum)
   (user-limit :type fixnum)))

(define-converters (%to-json from-json) voice-channel
  (topic)
  (bitrate)
  (user-limit))

(defclass* news-channel (guild-channel)
  ((topic        :type (or null string))
   (last-message-id :type (or null snowflake))))

(define-converters (%to-json from-json) news-channel
  (topic)
  (last-message-id '%maybe-sf))

(defclass* store-channel (guild-channel) ())

(defclass* dm-channel (base-channel)
  ((last-message-id :type (or null snowflake))
   (recipients   :type (vector user))))

(define-converters (%to-json from-json) dm-channel
  (recipients (caching-vector-reader 'user))
  (last-message-id '%maybe-sf))

(defclass* group-dm (dm-channel)
  ((name     :type string)
   (icon     :type (or null string))
   (owner-id :type snowflake)))

(define-converters (%to-json from-json) group-dm
  (name)
  (icon)
  (owner-id 'parse-snowflake))

(export-pub channel)
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
