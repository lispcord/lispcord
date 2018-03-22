(asdf:defsystem :example-bot
    :description "An example client"
    :author "spreadLink"
    :maintainer "spreadLink"
    :license "Public Domain"
    :serial t
    :depends-on (lispcord)
    :components
    ((:module src
	      :serial t
	      :components
	      ((:file "package")
	       (:file "commands")
	       (:file "main")))))
