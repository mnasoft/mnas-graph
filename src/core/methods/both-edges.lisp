;;;; ./src/core/methods/both-edges.lisp

(in-package #:mnas-graph)

(defmethod both-edges ((node <node>) (graph <graph>) &aux (ht (make-hash-table)))
  "
 @b(Пример использования:)
@begin[lang=lisp](code)
  (let ((graph (mnas-graph:make-graph
                '((\"a\" \"c\") (\"c\" \"d\") (\"c\" \"g\") (\"c\" \"e\")
                  (\"e\" \"f\") (\"e\" \"g\") (\"h\" \"j\") (\"b\" \"f\"))
                :nodes '(\"k\"))))
    (both-edges (mnas-graph:find-node \"c\" graph) graph))
@end(code)
"
  (when (into-container-p node graph)
    (loop :for edge :being :the :hash-keys :in (ht-outlet-edges node) :do
      (setf (gethash edge ht)  nil))
    (loop :for edge :being :the :hash-keys :in (ht-inlet-edges  node) :do
      (setf (gethash edge ht)  nil)))
  ht)

(defmethod both-edges ((node string) (graph <graph>))
"
 @b(Пример использования:)
@begin[lang=lisp](code)
  (let ((graph (mnas-graph:make-graph
                '((\"a\" \"c\") (\"c\" \"d\") (\"c\" \"g\") (\"c\" \"e\")
                  (\"e\" \"f\") (\"e\" \"g\") (\"h\" \"j\") (\"b\" \"f\"))
                :nodes '(\"k\"))))
    (both-edges \"c\"  graph))
@end(code)
"
  (both-edges (find-node node graph)))
