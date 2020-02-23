(in-package :lispcord.classes)

(defmethod channels ((cat category))
  (let ((g (guild cat)))
    (remove-if-not (lambda (chan)
                     (eql (id cat)
                          (parent-id chan)))
                   (channels g))))

(defmethod overwrite ((c guild-channel) (_ (eql :everyone)))
  "Permission overwrite for @everyone"
  (find (guild-id c) (overwrites c) :key 'id))

(defmethod overwrite ((c guild-channel) (m role))
  "Permission overwrite for role in channel"
  (find (id m) (overwrites c) :key 'id))

(defmethod overwrite ((c guild-channel) (m member))
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
(defmethod role-everyone ((g available-guild))
  "Returns the @everyone role of the guild"
  ;; It always has the same id as the guild
  (getcache-id (id g) :role))

(defmethod member ((u user) &optional g)
  (when (typep g 'available-guild)
    (find-if (lambda (e) (eq u (lc:user e))) (members g))))

(export-pub nick-or-name)
(defgeneric nick-or-name (user message-or-guild)
  (:documentation "If `user` has a nickname on guild then return it, otherwise return the user's username"))

(defmethod nick-or-name ((u user) (m message))
  "Member u of the guild with message m"
  (let ((c (channel m)))
    (if (typep c 'guild-channel)
        (nick-or-name u (guild c))
        (name u))))

(defmethod nick-or-name ((u user) (g unavailable-guild))
    "Unavailable guild, so can't get member objects"
  (name u))

(defmethod nick-or-name ((u user) (g available-guild))
  "Member u of the guild g"
  (if-let ((member (member u g)))
    (or (nick member)
        (name u))
    (name u)))

(define-alias overwrites permission-overwrites)
(define-alias version v)

;;;; Deprecated accessors

(defmethod available ((g base-guild))
  (not (unavailable-p g)))

(defmethod channels ((r ready))
  (private-channels r))

(defmethod (setf channels) (new-value (r ready))
  (setf (private-channels r) new-value))

(defmethod name ((u user))
  (username u))

(defmethod (setf name) (new-value (u user))
  (setf (username u) new-value))

(defmethod file ((a attachment))
  (filename a))

(defmethod (setf file) (new-value (a attachment))
  (setf (filename a) new-value))

(defmethod icon ((i embed-footer))
  (icon-url i))

(defmethod (setf icon) (new-value (i embed-footer))
  (setf (icon-url i) new-value))

(defmethod emailp ((u user))
  (not (null (email u))))

(define-alias last-message last-message-id)
(define-alias last-pinned last-pin-timestamp)
(define-alias icon-proxy-url proxy-icon-url)
(define-alias proxy-icon proxy-icon-url)
(define-alias colonsp require-colons-p)
(define-alias managedp managed-p)
(define-alias animatedp animated-p)
(define-alias hoistp hoist-p)
(define-alias mentionablep mentionable-p)
(define-alias deafp deaf-p)
(define-alias mutep mute-p)
(define-alias afk-to afk-timeout)
(define-alias embedp embed-enabled-p)
(define-alias verify-level verification-level)
(define-alias notify-level default-message-notifications)
(define-alias app-id application-id)
(define-alias widgetp widget-enabled-p)
(define-alias widget-id widget-channel-id)
(define-alias largep large-p)
(define-alias enabledp enabled-p)
(define-alias syncingp syncing-p)
(define-alias expire-grace expire-grace-period)
(define-alias editedp edited-timestamp)
(define-alias mention-all-p mention-everyone-p)
(define-alias pinnedp pinned-p)
(define-alias discrim discriminator)
(define-alias botp bot-p)
(define-alias mfap mfa-enabled-p)
(define-alias verifiedp verified-p)
