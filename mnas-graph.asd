;;;; mnas-graph.asd

(defsystem "mnas-graph"
  :description "Describe mnas-graph here"
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :version "0.0.3"
  :serial nil
  :in-order-to ((test-op (test-op "mnas-graph/tests")))
  :depends-on ("mnas-hash-table")
  :components ((:module "src"
		:serial nil
                :components ((:file "mnas-graph")))))

(defsystem "mnas-graph/view"
  :description "Describe mnas-graph here"
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"
  :depends-on ("mnas-graph" "mnas-graph/printer-viewer")
  :serial nil
  :in-order-to ((test-op (test-op "mnas-graph/tests")))
  :components ((:module "src/view"
		:serial nil
                :components ((:file "view")))))

(defsystem "mnas-graph/printer-viewer"
  :description "Describe mnas-graph here"
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :serial nil
  :in-order-to ((test-op (test-op "mnas-graph/tests")))
  ;; :depends-on ("mnas-hash-table")
  :components ((:module "src/printer-viewer"
		:serial nil
                :components ((:file "printer-viewer")))))

(defsystem "mnas-graph/filter"
  :description "Describe mnas-graph here"
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :serial nil
  :in-order-to ((test-op (test-op "mnas-graph/tests")))
  ;; :depends-on ("mnas-hash-table")
  :components ((:module "src/filter"
		:serial nil
                :components ((:file "filter")))))

(defsystem "mnas-graph/demos"
  :description "Тестирование систем, входящих  в проект mnas-graph"
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :depends-on ("mnas-graph" "mnas-graph/view")
  :components ((:module "src/demos"
		:serial nil
                :components ((:file "demos")
                             ))))

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
