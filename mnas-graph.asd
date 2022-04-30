;;;; mnas-graph.asd

(defsystem "mnas-graph"
  :description
  " System mnas-graph defines basic functions for creating a graph
 data structure and displaying it via graphviz."
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :version "0.0.5"
  :serial nil
  :in-order-to ((test-op (test-op "mnas-graph/tests")))
  :depends-on ("mnas-graph/core" "mnas-graph/view" "mnas-graph/demos" "mnas-hash-table"))

(defsystem "mnas-graph/core"
  :description "Describe mnas-graph here"
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :serial nil
  :in-order-to ((test-op (test-op "mnas-graph/tests")))
  :depends-on ("mnas-hash-table")
  :components ((:module "src/core"
		:serial nil
                :components ((:file "mnas-graph")))))

(defsystem "mnas-graph/view"
  :description "Describe mnas-graph here"
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"
  :depends-on ("mnas-graph/core" "mnas-graph/printer-viewer" "mnas-graph/filter")
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
  :depends-on ("mnas-graph/core" "mnas-graph/view")
  :components ((:module "src/demos"
		:serial nil
                :components ((:file "demos")))))

(defsystem "dxf/tests"
  :description "Тестирование систем, входящих  в проект mnas-graph"
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :depends-on ("dxf" "fiveam")
  :perform (test-op (o s)
		    (uiop:symbol-call :dxf/tests :run-tests))
  :components ((:module "src/tests"
		:serial nil
                :components ((:file "tests")))
               (:module "src/tests/suites"
                :depends-on ("src/tests")
		:serial nil
                :components ((:file "main")
                             ))
               (:module "src/tests/run"
                :depends-on ("src/tests/suites")
		:serial nil
                :components ((:file "run")))))

(defsystem "mnas-graph/docs"
  :description "Зависимости для сборки документации"
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"
  :depends-on ("mnas-graph" "codex" "mnas-package")
  :components ((:module "src/docs"
		:serial nil
                :components ((:file "docs")))))
