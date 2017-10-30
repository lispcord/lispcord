(asdf:defsystem :lispcord
    :description "A client library for the discordapp bot api"
    :author "MegaLoler"
    :license "yet-to-be-specified"
    :serial t
    :depends-on (:dexador
		 :websocket-driver-client
		 :jonathan
		 :bordeaux-threads)
    :components ((:module src
			  :serial t
			  :components
			  ((:file "package")
			   (:file "util")
			   (:file "core")
			   (:module payloads
				    :serial t
				    :components
				    ((:file roles)
				     (:file user)
				     (:file channel)
				     (:file emoji)
				     (:file guilds)
				     (:file ready)))
			   (:file "http")
			   (:file "gateway")
			   (:file "lispcord")))))
