;;;; ekzperiment.asd

(asdf:defsystem #:ekzperiment
  :description "Describe ekzperiment here"
  :author "Christian Mehrstam"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:cl-store #:cl-blt)
  :components ((:file "package")
	       (:file "base")
	       (:file "printing")
	       (:file "levels")
	       (:file "inventory")
	       (:file "item-handling")
	       (:file "render")
	       (:file "astar")
	       (:file "fov")
               (:file "ekzperiment")))
