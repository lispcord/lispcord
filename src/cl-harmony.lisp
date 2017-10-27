
(defparameter gateway-url-suffix "/?v=6&encoding=json")


;; (TODO:) we should move this into a util.lisp file eventually
(defun str-concat (&rest strings)
  (apply #'concatenate 'string strings))


(defstruct (bot (:constructor primitive-bot-make))
  (token "" :type string :read-only t)
  (version "0.0.1" :type string)
  (url "N/A" :type string)
  (base-url "" :type string))

(defun make-bot (token &key
			 (version "0.0.1")
			 (url "N/A")
			 (api-ver "v6"))
  (unless token (error "No token specified!"))
  (primitive-bot-make :token token
		      :version version
		      :url url
		      :base-url (str-concat "https://discordapp.com/api/"
					    api-ver
					    "/")))

(defun user-agent (bot)
  (str-concat "DiscordBot (" (bot-url bot) ", " (bot-version bot) ")"))

(defun headers (bot &optional (length 0))
  (list (cons "Authorization" (str-concat "Bot " (bot-token bot)))
        (cons "User-Agent" (user-agent bot))
        (cons "Content-length" (format nil "~a" length))))

;; is 'get' reserved?
(defun get-rq (endpoint)
  (dex:get (str-concat discord-api-base-url "/" endpoint)))

;; i added -rq to make the name match get-rq for now
(defun post-rq (endpoint token)
  (dex:post (str-concat discord-api-base-url "/" endpoint)
	    :headers (mk-headers token)))

(defun gateway ()
  (get-rq "gateway"))

(defun fetch-gateway-url ()
  (cadr (jonathan:parse (gateway))))

;; should this be combined with the above?
(defun gateway-url ()
  (str-concat (fetch-gateway-url) gateway-url-suffix))

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
