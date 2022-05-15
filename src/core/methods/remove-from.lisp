;;;; ./src/core/methods/remove-from.lisp

(in-package #:mnas-graph)

(defmethod remove-from ((node <node>) (graph <graph> ))
  "
 @b(Пример использования:)
@begin[lang=lisp](code)
  (let ((graph (mnas-graph:make-graph
                '((\"a\" \"c\") (\"c\" \"d\") (\"c\" \"g\") (\"c\" \"e\")
                  (\"e\" \"f\") (\"e\" \"g\") (\"h\" \"j\") (\"b\" \"f\"))
                :nodes '(\"k\"))))
    (remove-from (mnas-graph:find-node \"c\" graph) graph))
@end(code)
"
  (when (into-container-p node graph)
    (loop :for edge :being :the :hash-keys :in (outlet-edges node graph) :do
      (remove-from edge graph))
    (loop :for edge :being :the :hash-keys :in (inlet-edges  node graph) :do
      (remove-from edge graph))
    (remhash node (nodes graph)))
  node)

(defmethod remove-from ((edge <edge>) (graph <graph>))
  "
 @b(Пример использования:)
@begin[lang=lisp](code)
  (let ((graph (mnas-graph:make-graph
                '((\"a\" \"c\") (\"c\" \"d\") (\"c\" \"g\") (\"c\" \"e\")
                  (\"e\" \"f\") (\"e\" \"g\") (\"h\" \"j\") (\"b\" \"f\"))
                :nodes '(\"k\"))))
    (remove-from (mnas-graph:find-edge \"c->d\" graph) graph))
@end(code)
"
  (when (into-container-p edge graph)
    (remhash (tail edge) (ht-outlet-edges (tail edge)))
    (remhash (head edge) (ht-inlet-edges  (head edge)))
    (remhash edge (edges graph))
    edge))

(defmethod remove-from ((name string) (graph <graph>)
                        &aux
                          (node (find-node name graph))
                          (edge (find-edge name graph)))
  "
 @b(Пример использования:)
@begin[lang=lisp](code)
  (let ((graph (mnas-graph:make-graph
                '((\"a\" \"c\") (\"c\" \"d\") (\"c\" \"g\") (\"c\" \"e\")
                  (\"e\" \"f\") (\"e\" \"g\") (\"h\" \"j\") (\"b\" \"f\"))
                :nodes '(\"k\"))))
    (list
       (remove-from  \"c\" graph)
       (remove-from  \"b->f\" graph)))
@end(code)
"  
  (cond
    (node (remove-from node graph))
    (edge (remove-from node graph))))
