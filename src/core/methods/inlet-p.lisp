;;;; ./src/core/methods/inlet-p.lisp

(in-package :mnas-graph)

(defmethod inlet-p ((node <node>) (graph <graph>))
  "
 @b(Пример использования:)
@begin[lang=lisp](code)
  (let ((graph (mnas-graph:make-graph
                '((\"a\" \"c\") (\"c\" \"d\") (\"c\" \"g\") (\"c\" \"e\")
                  (\"e\" \"f\") (\"e\" \"g\") (\"h\" \"j\") (\"b\" \"f\"))
                :nodes '(\"k\"))))
    (inlet-p (mnas-graph:find-node \"d\" graph) graph))
@end(code)

"
  (let ((inlet  (hash-table-count (find-backward-nodes node graph)))
        (outlet (hash-table-count (find-forward-nodes  node graph))))
    (when (and (< 0 inlet) (= 0 outlet))
      t)))

(defmethod inlet-p ((node string) (graph <graph>))
    "
   @b(Пример использования:)
@begin[lang=lisp](code)
  (let ((graph (mnas-graph:make-graph
                '((\"a\" \"c\") (\"c\" \"d\") (\"c\" \"g\") (\"c\" \"e\")
                  (\"e\" \"f\") (\"e\" \"g\") (\"h\" \"j\") (\"b\" \"f\"))
                :nodes '(\"k\"))))
    (inlet-p \"d\" graph))
@end(code)
"
  (inlet-p (find-node node graph) graph))
