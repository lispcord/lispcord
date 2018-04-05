(asdf:defsystem :lispcord
    :description "A client library for the discordapp bot api"
    :author "MegaLoler, spreadLink"
    :maintainer "spreadLink"
    :license "MIT"
    :serial t
    :depends-on (:drakma
     :babel
     :websocket-driver-client
     :jonathan
     :bordeaux-threads)
    :components ((:module src
        :serial t
        :components
        ((:file "util")
         (:module classes
            :serial t
            :components
            ((:file "package")
             (:file "core")
             (:file "cache")
             (:file "emoji")
             (:file "user")
             (:file "integration")
             (:file "channel")
             (:file "guild")
             (:file "embed")
             (:file "message")))
         (:file "package")
         (:file "ratelimits")
         (:file "constants")
         (:file "core")
         (:file "pipes")
         (:module http
            :serial t
            :components
            ((:file "core")
             (:file "channel")
             (:file "emoji")
             (:file "guild")
             (:file "user")))
         (:file "gateway")
         (:file "lispcord")))))
