(ql:quickload :dexador)
(ql:quickload :websocket-driver-client)
(ql:quickload :jonathan)

(defparameter bot-url "N/A")
(defparameter bot-ver-str "0.0.1")
(defparameter discord-api-base-url "https://discordapp.com/api/v6")
(defparameter gateway-url-suffix "/?v=6&encoding=json")

(defun str-concat (&rest strings)
  (apply #'concatenate 'string strings))

;; unsure if the url and version string should be quoted or not in the header
(defun mk-user-agent-str ()
  (str-concat "DiscordBot ('" bot-url "', '" bot-ver-str "')"))

;; is there a better way to do this? quote was causing str-concat to be literal
(defun mk-headers (token)
  (list (cons "Authorization" (str-concat "Bot " token))
        (cons "User-Agent" (mk-user-agent-str))
        (cons "Content-length" "0")))

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
            (print (str-concat "ws: " message)))))
