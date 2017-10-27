(ql:quickload "dexador")

;; should these be constants?
(defparameter bot-url "N/A")
(defparameter bot-ver-str "0.0.1")
(defparameter discord-api-base-url "https://discordapp.com/api/v6")

(defun str-concat (&rest strings)
  (apply #'concatenate 'string strings))

;; unsure if the url and version string should be quoted or not in the header
(defun mk-user-agent-str ()
  (str-concat "DiscordBot ('" bot-url "', '" bot-ver-str "')"))

;; not sure if this should exist or not, a part of me just wants to be explicit
(defun mk-header (key val)
  (cons key val))

;; is there a better way to do this? quote was causing str-concat to be literal
(defun mk-headers (token)
  (list (mk-header "Authorization" (str-concat "Bot " token))
        (mk-header "User-Agent" (mk-user-agent-str))
        (mk-header "Content-length" "0")))

;; is 'get' reserved?
(defun get-rq (endpoint)
  (dex:get (str-concat discord-api-base-url "/" endpoint)))

;; i added -rq to make the name match get-rq for now
(defun post-rq (endpoint token)
  (dex:post (str-concat discord-api-base-url "/" endpoint)
	    :headers (mk-headers token)))

(defun gateway ()
  (get-rq "gateway"))
