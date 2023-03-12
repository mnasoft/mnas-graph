;;;; ./src/core/methods/find-backward-nodes.lisp

(in-package :mnas-graph)

(defmethod find-backward-nodes (node (graph <graph>))
  (make-hash-table))

(defmethod find-backward-nodes ((node string) (graph <graph>))
  "
   @b(Пример использования:)
@begin[lang=lisp](code)
  (let ((graph (mnas-graph:make-graph
                '((\"a\" \"c\") (\"c\" \"d\") (\"c\" \"g\") (\"c\" \"e\")
                  (\"e\" \"f\") (\"e\" \"g\") (\"h\" \"j\") (\"b\" \"f\"))
                :nodes '(\"k\"))))
    (mnas-graph:find-backward-nodes \"c\" graph))
@end(code)
"
  (find-backward-nodes (find-node node graph) graph))

(defmethod find-backward-nodes  ((node <node>) (graph <graph>)
                                 &aux (ht (make-hash-table)))
  "
 @b(Пример использования:)
@begin[lang=lisp](code)
  (let ((graph (mnas-graph:make-graph
                '((\"a\" \"c\") (\"c\" \"d\") (\"c\" \"g\") (\"c\" \"e\")
                  (\"e\" \"f\") (\"e\" \"g\") (\"h\" \"j\") (\"b\" \"f\"))
                :nodes '(\"k\"))))
    (mnas-graph:find-backward-nodes
     (mnas-graph:find-node \"c\" graph) graph))
@end(code)
"
  (when (into-container-p node graph)
    (loop :for edge :being :the :hash-keys :in (ht-inlet-edges node) :do
      (setf (gethash (tail edge) ht) nil)))
  ht)
