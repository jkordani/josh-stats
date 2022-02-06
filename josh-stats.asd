;;;; josh-stats.asd

(asdf:defsystem #:josh-stats
  :description "Describe josh-stats here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:vgplot)
  :components ((:file "package")
               (:file "josh-stats")))
