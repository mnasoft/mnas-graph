;;;; mnas-graph.asd

(asdf:defsystem #:mnas-graph
  :description "Describe mnas-graph here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:mnas-hash-table)
  :components ((:file "mnas-graph")
	       (:file "demos")
	       ))
