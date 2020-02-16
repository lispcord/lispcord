(asdf:defsystem :lispcord
  :description "A client library for the discordapp bot api"
  :author "MegaLoler, spreadLink"
  :maintainer "spreadLink"
  :license "MIT"
  :depends-on (:drakma
               :alexandria
               :verbose
               :split-sequence
               :babel
               :websocket-driver-client
               :jonathan
               :bordeaux-threads)

  :serial t
  :pathname "src"
  :components ((:file "util")
               (:module classes
                :serial t
                :components
                ((:file "package")
                 (:file "definer")
                 (:file "core")
                 (:file "permissions")
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
               (:file "lispcord"))

  :in-order-to ((asdf:test-op (asdf:test-op :lispcord-tests))))

(asdf:defsystem #:lispcord-tests
  :depends-on (:lispcord :parachute)
  :pathname "t"
  :serial t
  :components ((:file "package")
               (:file "classes-definer"))
  :perform (test-op (o s)
                    (uiop:symbol-call :parachute :test :lispcord-tests)))
