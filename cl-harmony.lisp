(ql:quickload "dexador")

(dex:post "https://discordapp.com/api/v6/"
          :headers '(("Authorization" . "Bot ___")
		     ("User-Agent" . "DiscordBot ($url, $versionNumber)")))
