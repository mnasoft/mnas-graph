;;;; ./src/core/methods/find-node.lisp

(in-package :mnas-graph)

(defmethod find-node ((name string) (graph <graph>))
  "
 @b(Пример использования:)
@begin[lang=lisp](code)
  (let ((graph (mnas-graph:make-graph
                '((\"a\" \"c\") (\"c\" \"d\") (\"c\" \"g\") (\"c\" \"e\")
                  (\"e\" \"f\") (\"e\" \"g\") (\"h\" \"j\") (\"b\" \"f\"))
                :nodes '(\"k\"))))
    (mnas-graph:find-node \"c\" graph))
@end(code)
"
  (gethash name (ht-node-names graph)))
