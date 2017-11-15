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
			   (:file "pipes")
			   (:module classes
				    :serial t
				    :components
				    ((:file "package")
				     (:file "core")
				     (:file "emoji")
				     (:file "user")
				     (:file "integration")
				     (:file "channel")
				     (:file "guild")
				     (:file "embed")
				     (:file "message")))
			   (:file "ratelimits")
			   (:file "constants")
			   (:file "core")
			   (:file "cache")
			   (:file "http")
			   (:file "gateway")
			   (:file "lispcord")
			   (:file "example")))))
