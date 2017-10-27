(ql:quickload "dexador")

;; should these be constants?
(defvar bot-url "N/A")
(defvar bot-ver-str "0.0.1")

(defun str-concat (&rest strings)
  (apply #'concatenate 'string strings))

;; unsure if the url and version string should be quoted or not
(defun mk-user-agent-str ()
  (str-concat "DiscordBot ('" bot-url "', '" bot-ver-str "')"))

(defun mk-headers (token)
  (list (cons "Authorization" (str-concat "Bot " token))
        (cons "User-Agent" (mk-user-agent-str))
        (cons "Content-length" "0")))

(defun post (endpoint token)
  (dex:post (str-concat "https://discordapp.com/api/v6/" endpoint)
	    :headers (mk-headers token)))

;; doesn't require token ?
(defun gateway ()
  (post "gateway" ""))
