;;;; mnas-graph.asd

(defsystem "mnas-graph"
  :description "Describe mnas-graph here"
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :version "0.0.2"
  :serial nil
  :in-order-to ((test-op (test-op "math/core/tests")))
  :depends-on ("mnas-hash-table")
  :components ((:module "src"
		:serial nil
                :components ((:file "mnas-graph")
	       (:file "demos" :depends-on ("mnas-graph"))))))

(defsystem "mnas-graph/tests"
  :description "Тестирование систем, входящих  в проект mnas-graph"
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :depends-on ("mnas-graph" "fiveam")
  :perform (test-op (o s)
		    (uiop:symbol-call :mnas-package/tests :run-tests))
  :components ((:module "src/tests"
		:serial nil
                :components ((:file "package")
                             (:file "all"  :depends-on ("package"))
                             (:file "main" :depends-on ("all"))
                             (:file "run"  :depends-on ("obj" "pkg" "make" "view" "main"))
                             ))))

(defsystem "mnas-graph/docs"
  :description "Зависимости для сборки документации"
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :depends-on ("mnas-package" "codex"))
