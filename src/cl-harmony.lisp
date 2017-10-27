(ql:quickload "dexador")


(defun ++ (&rest strings)
  (apply #'concatenate 'string strings))

(defun post (endpoint token)
  (dex:post (++ "https://discordapp.com/api/v6/" endpoint)
	    :headers '(("Authorization" . (++ "Bot " token))
		       ("User-Agent" . "DiscordBot ($url, $versionNumber)"))))
