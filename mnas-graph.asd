;;;; mnas-graph.asd

(defsystem "mnas-graph"
  :description
  " Система @b(mnas-graph) определяет базовые функции для создания
 структуры данных типа
 @link[uri=\"https://ru.wikipedia.org/wiki/Граф_(математика)\"](Граф).

 Проект определяет следующие основные классы: @begin(list)
@item(@ref[id=class-node](<node>) - вершина графа;)
@item(@ref[id=class-edge](<edge>) - ребро графа;)
@item(@ref[id=class-graph](<graph>) - граф.)  @end(list)

 @b(Пример использования:)
@begin[lang=lisp](code)
(let*
  ((g (make-instance 'mnas-graph:<graph>))
   (v1 (make-instance 'mnas-graph:<node> :owner g :name \"v1\"))
   (v2 (make-instance 'mnas-graph:<node> :owner g :name \"v2\"))
   (v3 (make-instance 'mnas-graph:<node> :owner g :name \"v3\"))
   (r1 (make-instance 'mnas-graph:<edge> :from v1 :to v2))
   (r2 (make-instance 'mnas-graph:<edge> :from v2 :to v3))
   (r3 (make-instance 'mnas-graph:<edge> :from v3 :to v1)))
 (mnas-graph:insert-to v1 g)
 (mnas-graph:insert-to v2 g)
 (mnas-graph:insert-to v3 g)
 (mnas-graph:insert-to r1 g)
 (mnas-graph:insert-to r2 g)
 (mnas-graph:insert-to r3 g)
 (mnas-graph:view-graph g))
@end(code)
и отображения через graphviz.
"
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :version "0.0.4"
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
  :depends-on ("mnas-graph" "mnas-graph/printer-viewer" "mnas-graph/filter")
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
                :components ((:file "demos")))))

(defsystem "mnas-graph/tests"
  :description "Тестирование систем, входящих  в проект mnas-graph"
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :depends-on ("mnas-graph/view" "fiveam")
  :perform (test-op (o s)
		    (uiop:symbol-call :mnas-graph/tests :run-tests))
  :components ((:module "src/tests"
		:serial nil
                :components ((:file "package")
                             (:file "all"  :depends-on ("package"))
                             (:file "main" :depends-on ("all"))
                             (:file "run"  :depends-on ("main"))
                             ))))

(defsystem "mnas-graph/docs"
  :description "Зависимости для сборки документации"
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :depends-on ("mnas-graph/view" "mnas-graph/demos" "mnas-package" "codex"))
