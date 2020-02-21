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

(define-converters (overwrite)
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
  name position topic nsfw bitrate user-limit permission-overwrites parent-id type)

(defclass* channel ()
  ((id :type snowflake)
   (type :type fixnum)))

(define-converters (channel)
  (id 'parse-snowflake)
  type)

(defclass* guild-channel (channel)
  ((guild-id      :type (or null snowflake)) ;; It's not present in GUILD_CREATE events
   (name          :type string)
   (position      :type fixnum)
   (permission-overwrites :type (vector overwrite))
   (parent-id     :type (or null snowflake))
   (nsfw          :type boolean :accessor nsfw-p)))

(define-converters (guild-channel)
  (guild-id '%maybe-sf)
  (name)
  (position)
  (permission-overwrites (subtable-vector-reader 'overwrite))
  (parent-id '%maybe-sf)
  (nsfw))

(defclass* category (guild-channel) ())

(defclass* text-channel (guild-channel)
  ((rate-limit-per-user :type fixnum)
   (topic               :type (or null string))
   (last-message-id     :type (or null snowflake))
   (last-pin-timestamp  :type (or null string))))

(define-converters (text-channel)
  (rate-limit-per-user)
  (topic)
  (last-message-id '%maybe-sf)
  (last-pin-timestamp))

(defclass* voice-channel (guild-channel)
  ((topic      :type (or null string))
   (bitrate    :type fixnum)
   (user-limit :type fixnum)))

(define-converters (voice-channel)
  (topic)
  (bitrate)
  (user-limit))

(defclass* news-channel (guild-channel)
  ((topic        :type (or null string))
   (last-message-id :type (or null snowflake))))

(define-converters (news-channel)
  (topic)
  (last-message-id '%maybe-sf))

(defclass* store-channel (guild-channel) ())

(defclass* dm-channel (channel)
  ((last-message-id :type (or null snowflake))
   (recipients   :type (vector user))))

(define-converters (dm-channel)
  (recipients (caching-vector-reader 'user))
  (last-message-id '%maybe-sf))

(defclass* group-dm (dm-channel)
  ((name     :type string)
   (icon     :type (or null string))
   (owner-id :type snowflake)))

(define-converters (group-dm)
  (name)
  (icon)
  (owner-id 'parse-snowflake))

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
