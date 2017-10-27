(ql:quickload "dexador")


(defun post (endpoint)
  (dex:post (concatenate 'string  "https://discordapp.com/api/v6/" endpoint)
	    :headers '(("Authorization" . "Bot ___")
		       ("User-Agent" . "DiscordBot ($url, $versionNumber)"))))
