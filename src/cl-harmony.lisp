(in-package :cl-harmony)

;; (TODO:) we should move this into a util.lisp file eventually
(defun str-concat (&rest strings)
  (apply #'concatenate 'string strings))

(defparameter bot-url "N/A")
(defun bot-url (url)
  (setf bot-url url))

(defparameter base-url "https://discordapp.com/api/v6/")
(defun api-version (version)
  (str-concat "https://discordapp.com/api/" version "/"))

(defparameter api-suffix "/?v=6&encoding=json")

(defstruct (bot (:constructor primitive-bot-make))
  (token "" :type string :read-only t)
  (version "0.0.1" :type string))

(defun make-bot (token &key (version "0.0.1"))
  (unless token (error "No token specified!"))
  (primitive-bot-make :token token
		      :version version))

(defun user-agent (bot)
  (str-concat "DiscordBot (" bot-url ", " (bot-version bot) ")"))

(defun headers (bot &optional (length 0))
  (list (cons "Authorization" (str-concat "Bot " (bot-token bot)))
        (cons "User-Agent" (user-agent bot))
        (cons "Content-length" (format nil "~a" length))))

;; is 'get' reserved?
(defun get-rq (endpoint)
  (dex:get (str-concat base-url "/" endpoint api-suffix)))

;; i added -rq to make the name match get-rq for now
(defun post-rq (bot endpoint)
  (dex:post (str-concat base-url "/" endpoint api-suffix)
	    :headers (headers bot)))

(defun gateway ()
  (get-rq "gateway"))

(defun fetch-gateway-url ()
  (cadr (jonathan:parse (gateway))))

;; should this be combined with the above?
(defun gateway-url ()
  (str-concat (fetch-gateway-url) api-suffix))

(defun make-ws-client (url)
  (wsd:make-client url))

;; i cant think off the top of my head the best way to structure these things
;; a lot of these can probably be merged
;; or better named
(defun get-ws-client ()
  (make-ws-client (gateway-url)))

;; im note sure what the *var* convention is, so im just going by the examples :p
(defun connect ()
  (defvar *client* (get-ws-client))
  (wsd:start-connection *client*)
  (wsd:on :message *client*
          (lambda (message)
	    (on-recv (jonathan:parse message)))))

;; receive message from websock
(defun on-recv (msg)
  (print msg))
