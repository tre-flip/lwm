;;;; lwm.asd

(asdf:defsystem #:lwm
  :description "Describe lwm here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on ("clx" "slynk")
  :components ((:file "package")
	       (:file "keys")
	       (:file "windows")
	       (:file "handlers")
	       (:file "lwm")))
