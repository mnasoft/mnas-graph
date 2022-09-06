;;;; ./src/core/methods/empty.lisp

(in-package #:mnas-graph)

(defmethod empty ((graph <graph>))
    "
 @b(Пример использования:)
@begin[lang=lisp](code)
  (let ((graph (mnas-graph:make-graph
              '((\"a\" \"c\") (\"c\" \"d\") (\"c\" \"g\") (\"c\" \"e\")
                (\"e\" \"f\") (\"e\" \"g\") (\"h\" \"j\") (\"b\" \"f\"))
              :nodes '(\"k\"))))
  (mnas-graph:empty graph))
  (let ((graph (mnas-graph:make-graph '())))
    (mnas-graph:empty graph))
"
  (when (= 0 (count-nodes graph)) t))
