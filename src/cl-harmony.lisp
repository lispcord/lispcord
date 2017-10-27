
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
  (dex:get (mk-api-url endpoint)))

(defun post-rq (endpoint token)
  (dex:post (mk-api-url endpoint)
	    :headers (mk-headers token)))

(defun gateway ()
  (get-rq "gateway"))
