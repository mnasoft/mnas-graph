;;;; ./src/core/methods/find-forward-nodes.lisp

(in-package #:mnas-graph)

(defmethod find-forward-nodes (node (graph <graph>))
  (make-hash-table))

(defmethod find-forward-nodes ((node string) (graph <graph>))
  "
  @b(Пример использования:)
@begin[lang=lisp](code)
  (let ((graph (mnas-graph:make-graph
                '((\"a\" \"c\") (\"c\" \"d\") (\"c\" \"g\") (\"c\" \"e\")
                  (\"e\" \"f\") (\"e\" \"g\") (\"h\" \"j\") (\"b\" \"f\"))
                :nodes '(\"k\"))))
    (mnas-graph:find-forward-nodes \"c\" graph))
@end(code)
"
  (find-forward-nodes (find-node node graph) graph))

(defmethod find-forward-nodes  ((node <node>) (graph <graph>) &aux (ht (make-hash-table)))
  "
 @b(Пример использования:)
@begin[lang=lisp](code)
  (let ((graph (mnas-graph:make-graph
                '((\"a\" \"c\") (\"c\" \"d\") (\"c\" \"g\") (\"c\" \"e\")
                  (\"e\" \"f\") (\"e\" \"g\") (\"h\" \"j\") (\"b\" \"f\"))
                :nodes '(\"k\"))))
    (mnas-graph:find-forward-nodes
     (mnas-graph:find-node \"c\" graph) graph))
@end(code)"
  (maphash
   #'(lambda (key val)
       val
       (setf (gethash (head key) ht) (head key)))
   (outlet-edges node graph))
  ht)
