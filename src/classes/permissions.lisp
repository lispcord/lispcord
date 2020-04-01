
(in-package :lispcord.classes)

(defclass permissions ()
  ((value :initarg :value :initform 0 :type fixnum :accessor value)))

(defmethod %to-json ((r permissions))
  (write-value (value permissions)))

(defparameter *permission-values* '((:create-instant-invite . #x00000001)
                                    (:kick-members          . #x00000002)
                                    (:ban-members           . #x00000004)
                                    (:administrator         . #x00000008)
                                    (:manage-channels       . #x00000010)
                                    (:manage-guild          . #x00000020)
                                    (:add-reactions         . #x00000040)
                                    (:view-audit-log        . #x00000080)
                                    (:priority-speaker      . #x00000100)
                                    (:stream                . #x00000200)
                                    (:view-channel          . #x00000400)
                                    (:send-messages         . #x00000800)
                                    (:send-tts-messages     . #x00001000)
                                    (:manage-messages       . #x00002000)
                                    (:embed-links           . #x00004000)
                                    (:attach-files          . #x00008000)
                                    (:read-message-history  . #x00010000)
                                    (:mention-everyone      . #x00020000)
                                    (:use-external-emojis   . #x00040000)
                                    (:connect               . #x00100000)
                                    (:speak                 . #x00200000)
                                    (:mute-members          . #x00400000)
                                    (:deafen-members        . #x00800000)
                                    (:move-members          . #x01000000)
                                    (:use-vad               . #x02000000)
                                    (:change-nickname       . #x04000000)
                                    (:manage-nicknames      . #x08000000)
                                    (:manage-roles          . #x10000000)
                                    (:manage-webhooks       . #x20000000)
                                    (:manage-emojis         . #x40000000)))

(defun permission-value (key)
  (let ((pair (assoc key *permission-values*)))
    (if pair
        (cdr pair)
        (error "Unknown permission: ~S" key))))

(defparameter *all-permissions* (reduce 'logior (mapcar 'cdr *permission-values*)))

(defmethod make-permissions ((permissions list))
  (make-instance 'permissions
                 :value
                 (reduce #'logior
                         (mapcar 'permission-value permissions))))

(defmethod make-permissions ((value integer))
  (make-instance 'permissions :value value))

(defun permissions-add (permissions other)
  (make-permissions (logior (value permissions)
                            (value other))))

(defun permissions-remove (permissions other)
  (make-permissions (logandc2 (value permissions)
                              (value other))))

(defun permissions-overwrite (permissions overwrites)
  (if overwrites
      (permissions-add (permissions-remove permissions
                                           (deny overwrites))
                       (allow overwrites))
      permissions))

(export-pub has-permission)
(defgeneric has-permission (obj key &optional channel)
  (:documentation "If `obj` (user, member, or permissions) has `key` permission on `channel`"))

(defmethod has-permission ((p permissions) key &optional channel)
  "Returns if permissions object has `key` permission"
  (declare (ignore channel))
  (unless (listp key)
    (setf key (list key)))
  (every (lambda (k)
           (logtest (value p) (permission-value k)))
         key))

;;; Translation from
;;; https://discordapp.com/developers/docs/topics/permissions#permission-overwrites

(defun base-permissions (member)
  (let ((guild (guild member)))
    (if (owner guild)
        (make-permissions *all-permissions*)
        (let* ((role-everyone (role-everyone guild))
               (permissions (permissions role-everyone)))
          (loop for role across (roles member)
            do (setf permissions (permissions-add permissions (permissions role))))
          (if (has-permission permissions :administrator)
              (make-permissions *all-permissions*)
              permissions)))))

(defun compute-overwrites (permissions member channel)
  (if (has-permission permissions :administrator)
      (make-permissions *all-permissions*)
      (progn
        (when-let ((overwrite-everyone (overwrite channel :everyone)))
          (setf permissions (permissions-overwrite permissions overwrite-everyone)))
        (loop for role across (roles member)
           do (permissions-overwrite permissions (overwrite channel role)))
        (permissions-overwrite permissions (overwrite channel member)))))

