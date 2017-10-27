(asdf:defsystem :cl-harmony
    :description "A client library for the discordapp bot api"
    :author "MegaLoler"
    :license "yet-to-be-specified"
    :serial t
    :depends-on ("dexador")
    :components ((:module src
			  :serial t
			  :components ((:file "package")
				       (:file "cl-harmony")))))
