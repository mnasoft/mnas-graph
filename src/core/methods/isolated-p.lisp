;;;; ./src/core/methods/isolated-p.lisp

(in-package #:mnas-graph)

(defmethod isolated-p ((node <node>) (graph <graph>))
    "
   @b(Пример использования:)
@begin[lang=lisp](code)
  (let ((graph (mnas-graph:make-graph
                '((\"a\" \"c\") (\"c\" \"d\") (\"c\" \"g\") (\"c\" \"e\")
                  (\"e\" \"f\") (\"e\" \"g\") (\"h\" \"j\") (\"b\" \"f\"))
                :nodes '(\"k\"))))
    (isolated-p (mnas-graph:find-node \"k\" graph) graph))
@end(code)
"
  (let ((inlet  (hash-table-count (find-backward-nodes node graph)))
        (outlet (hash-table-count (find-forward-nodes  node graph))))
    (when (and (= 0 inlet) (= 0 outlet))
      t)))

(defmethod isolated-p ((node string) (graph <graph>))
    "
   @b(Пример использования:)
@begin[lang=lisp](code)
  (let ((graph (mnas-graph:make-graph
                '((\"a\" \"c\") (\"c\" \"d\") (\"c\" \"g\") (\"c\" \"e\")
                  (\"e\" \"f\") (\"e\" \"g\") (\"h\" \"j\") (\"b\" \"f\"))
                :nodes '(\"k\"))))
    (isolated-p \"k\" graph))
@end(code)
"
  (isolated-p (find-node node graph) graph))
