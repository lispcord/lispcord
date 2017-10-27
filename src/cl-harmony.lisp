(ql:quickload "dexador")

;; should these be constants?
(defvar bot-url "N/A")
(defvar bot-ver-str "0.0.1")
(defvar discord-api-ver "v6")
(defvar discord-api-base-url "https://discordapp.com/api")

(defun str-concat (&rest strings)
  (apply #'concatenate 'string strings))

;; unsure if the url and version string should be quoted or not in the header
(defun mk-user-agent-str ()
  (str-concat "DiscordBot ('" bot-url "', '" bot-ver-str "')"))

;; not sure if this should exist or not, a part of me just wants to be explicit
(defun mk-header (key val)
  (cons key val)

;; is there a better way to do this? quote was causing str-concat to be literal
(defun mk-headers (token)
  (list (mk-header "Authorization" (str-concat "Bot " token))
        (mk-header "User-Agent" (mk-user-agent-str))
        (mk-header "Content-length" "0")))

(defun mk-api-url (endpoint)
  (str-concat discord-api-base-url "/" discord-api-ver "/" endpoint))

;; is 'get' reserved?
(defun get-rq (endpoint)
  (dex:get (mk-api-url endpoint)))

;; i added -rq to make the name match get-rq for now
(defun post-rq (endpoint token)
  (dex:post (mk-api-url endpoint)
	    :headers (mk-headers token)))

(defun gateway ()
  (get-rq "gateway"))
