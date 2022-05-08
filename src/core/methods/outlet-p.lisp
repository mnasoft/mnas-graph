;;;; ./src/core/methods/outlet-p.lisp

(in-package #:mnas-graph)

(defmethod outlet-p ((node <node>) (graph <graph>))
  "
   @b(Пример использования:)
@begin[lang=lisp](code)
  (let ((graph (mnas-graph:make-graph
                '((\"a\" \"c\") (\"c\" \"d\") (\"c\" \"g\") (\"c\" \"e\")
                  (\"e\" \"f\") (\"e\" \"g\") (\"h\" \"j\") (\"b\" \"f\"))
                :nodes '(\"k\"))))
    (outlet-p (mnas-graph:find-node \"a\" graph) graph))
@end(code)
"
  (let ((inlet  (hash-table-count (find-backward-nodes node graph)))
        (outlet (hash-table-count (find-forward-nodes  node graph))))
    (when (and (< 0 outlet) (= 0 inlet))
      t)))

(defmethod outlet-p ((node string) (graph <graph>))
    "
   @b(Пример использования:)
@begin[lang=lisp](code)
  (let ((graph (mnas-graph:make-graph
                '((\"a\" \"c\") (\"c\" \"d\") (\"c\" \"g\") (\"c\" \"e\")
                  (\"e\" \"f\") (\"e\" \"g\") (\"h\" \"j\") (\"b\" \"f\"))
                :nodes '(\"k\"))))
    (outlet-p \"a\" graph))
@end(code)
"
  (outlet-p (find-node node graph) graph))
