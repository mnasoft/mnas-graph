;;;; mnas-graph.asd

(defsystem #:mnas-graph
  :description "Describe mnas-graph here"
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :version "0.0.1"
  :serial t
  :depends-on (#:mnas-hash-table)
  :components ((:file "package")
	       (:file "mnas-graph" :depends-on ("package"))
	       (:file "demos" :depends-on ("mnas-graph" "package"))
	       ))
