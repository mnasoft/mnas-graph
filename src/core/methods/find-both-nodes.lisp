;;;; ./src/core/methods/find-both-nodes.lisp

(in-package #:mnas-graph)

(defmethod find-both-nodes ((node string) (graph <graph>))
  "
   @b(Пример использования:)
@begin[lang=lisp](code)
  (let ((graph (mnas-graph:make-graph
                '((\"a\" \"c\") (\"c\" \"d\") (\"c\" \"g\") (\"c\" \"e\")
                  (\"e\" \"f\") (\"e\" \"g\") (\"h\" \"j\") (\"b\" \"f\"))
                :nodes '(\"k\"))))
    (mnas-graph:find-both-nodes \"c\" graph))
@end(code)
"
  (find-both-nodes (find-node node graph) graph))

(defmethod find-both-nodes  ((node <node>) (graph <graph>) &aux (ht (make-hash-table)))
  "
 @b(Пример использования:)
@begin[lang=lisp](code)
  (let ((graph (mnas-graph:make-graph
                '((\"a\" \"c\") (\"c\" \"d\") (\"c\" \"g\") (\"c\" \"e\")
                  (\"e\" \"f\") (\"e\" \"g\") (\"h\" \"j\") (\"b\" \"f\"))
                :nodes '(\"k\"))))
    (mnas-graph:find-both-nodes
     (mnas-graph:find-node \"c\" graph) graph))
@end(code)
"
  (maphash
   #'(lambda (key val)
       val
       (setf (gethash (head key) ht) (head key))
       (setf (gethash (tail key) ht) (tail key))
       (remhash node ht))
   (both-edges node))
  ht)
