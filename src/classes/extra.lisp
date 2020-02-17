(in-package :lispcord.classes)

(defmethod channels ((cat category))
  (let ((g (guild cat)))
    (remove-if-not (lambda (chan)
                     (eql (id cat)
                          (parent-id chan)))
                   (channels g))))

(defmethod overwrite ((c channel) (_ (eql :everyone)))
  "Permission overwrite for @everyone"
  (find (guild-id c) (overwrites c) :key 'id))

(defmethod overwrite ((c channel) (m role))
  "Permission overwrite for role in channel"
  (find (id m) (overwrites c) :key 'id))

(defmethod overwrite ((c channel) (m member))
  "Permission overwrite for member in channel"
  (find (id (user m)) (overwrites c) :key 'id))

(defmethod has-permission ((m member) key &optional channel)
  (let ((base-permissions (base-permissions m)))
    (if channel
        (has-permission (compute-overwrites base-permissions m channel) key)
        (has-permission base-permissions key))))

(defmethod has-permission ((u user) key &optional channel)
  (if channel
      (if-let ((member (member u (guild channel))))
        (has-permission member key channel)
        (error "User ~S is not a member of ~S" u channel))
      (error "Global users don't have permissions. Either replace the user object with member, or specify the channel.")))

(defmethod role ((i integration))
  (getcache-id (role-id i) :role))

(export-pub role-everyone)
(defmethod role-everyone ((g guild))
  "Returns the @everyone role of the guild"
  ;; It always has the same id as the guild
  (getcache-id (id g) :role))

(defmethod member ((u user) (g guild))
  (find-if (lambda (e) (eq u (lc:user e))) (members g)))

(defun user-or-webhook (obj)
  (if (gethash "webhook_id" obj)
      (from-json 'webhook obj)
      (cache 'user obj)))

(export-pub nick-or-name)
(defgeneric nick-or-name (user message-or-guild)
  (:documentation "If `user` has a nickname on guild then return it, otherwise return the user's username"))

(defmethod nick-or-name ((u user) (m message))
  "Member u of the guild with message m"
  (let ((c (channel m)))
    (if (typep c 'guild-channel)
        (nick-or-name u (guild c))
        (name u))))

(defmethod nick-or-name ((u user) (g  guild))
  "Member u of the guild g"
  (if-let ((member (member u g)))
    (or (nick member)
        (name u))
    (name u)))

(export-pub overwrites)
(defmethod overwrites ((c channel))
  (permission-overwrites c))

