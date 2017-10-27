(ql:quickload "dexador")

(dex:post "https://discordapp.com/api/v6/"
          :headers '(("Authorization" . "Bot MzY2Mjk4NDEyNDQzODkzNzYx.DNSRNA.T0iYDzRgD4Na_LubrYtVjLrOx3Y")
		     ("User-Agent" . "DiscordBot ($url, $versionNumber)")))
