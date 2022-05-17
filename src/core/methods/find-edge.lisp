;;;; ./src/core/methods/find-edge.lisp]]

(in-package #:mnas-graph)

(defmethod find-edge ((name string) (graph <graph>))
  "
 @b(Пример использования:)
@begin[lang=lisp](code)
  (let ((graph (mnas-graph:make-graph
                '((\"a\" \"c\") (\"c\" \"d\") (\"c\" \"g\") (\"c\" \"e\")
                  (\"e\" \"f\") (\"e\" \"g\") (\"h\" \"j\") (\"b\" \"f\"))
                :nodes '(\"k\"))))
    (mnas-graph:find-edge \"a->c\" graph))
@end(code)
"
  (gethash name (ht-edge-names graph)))
