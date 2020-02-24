(in-package :lispcord.core)

(defparameter *client* nil
  "This is an empty dummy var to allow for implicits.
It may be set by make-bot!")

(defstruct (bot (:constructor primitive-make-bot))
  (token "" :type string :read-only t)
  (user nil :type (or null lc:user))
  (version "0.0.1" :type string)
  (seq 0 :type fixnum)
  (session-id nil :type (or null string))
  (afk-since nil :type (or null fixnum))
  (event-handlers (make-hash-table) :type hash-table)
  conn
  (running nil :type boolean)
  (heartbeat-ack t :type boolean)
  (auth-as-bot t :type boolean)
  heartbeat-thread)


(defparameter *bot-url* "N/A")
(defun bot-url (url)
  (setf *bot-url* url))


(defun user-agent (bot)
  (str-concat "DiscordBot (" *bot-url* ", " (bot-version bot) ")"))

(defun bot-auth (bot)
  (str-concat (if (bot-auth-as-bot bot)
                  "Bot "
                  "")
              (bot-token bot)))

(defun headers (bot)
  (list (cons "Authorization" (bot-auth bot))))



(defmethod discord-req (endpoint
                    &key bot content
                      (content-type "application/json")
                      (type :get)
                      parameters
                    &aux
                      (url (str-concat +base-url+ endpoint))
                      (final (rl-buffer endpoint)))
  (v:debug :lispcord.core "~&HTTP-~a-Request to: ~a~@[~%  content: ~a~%~]"
          type url content)
  (multiple-value-bind (body status headers uri
                             stream closedp reason)
      (drakma:http-request
       url
       :method type
       :parameters parameters
       :content-type content-type
       :content content
       :user-agent (if bot (user-agent bot) :drakma)
       :additional-headers (if bot (headers bot))
       :external-format-in :utf8
       :external-format-out :utf8)
    (declare (ignore uri stream closedp reason))
    (rl-parse final headers)
    (let ((response (babel:octets-to-string body)))
      (case status
        (400 (cerror "ignore" "HTTP: BAD REQUEST~%~A" (jonathan:parse response :as :plist)))
        (401 (cerror "ignore" "HTTP: UNAUTHORIZED~%~A" (jonathan:parse response :as :plist)))
        (403 (cerror "ignore" "HTTP: FORBIDDEN~%~A" (jonathan:parse response :as :plist)))
        (405 (cerror "ignore" "HTTP: BAD METHOD~%~A" (jonathan:parse response :as :plist)))
        (408 (cerror "ignore" "HTTP: TIMEOUT~%~A" (jonathan:parse response :as :plist)))
        (429 (cerror "ignore" "HTTP: RATELIMIT~%~A" (jonathan:parse response :as :plist)))
        (520 (cerror "ignore" "HTTP: UNKNOWN~%~A" (jonathan:parse response :as :plist))))
      (values (cond ((= status 204) t)
                    ((= status 404) nil)
                    (t (jparse response)))
              status))))

(defun get-rq (endpoint &optional bot)
  (discord-req endpoint :bot bot :type :get))

(defun post-rq (endpoint &optional bot content)
  (discord-req endpoint :bot bot :content content :type :post))


