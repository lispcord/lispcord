(ql:quickload "dexador")


(defun str-concat (&rest strings)
  (apply #'concatenate 'string strings))

(defun post (endpoint token)
  (dex:post (str-concat "https://discordapp.com/api/v6/" endpoint)
	    :headers '(("Authorization" . (str-concat "Bot " token))
		       ("User-Agent" . "DiscordBot (N/A, 0.0.1)"))))
