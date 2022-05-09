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
  (let* ((rh (edges graph))
	 (rl (mnas-hash-table:hash-table-copy rh)))
    (maphash #'(lambda(key val)
		 val
		 (if (or
		      (eq (tail key) node)
		      (eq (head key)   node))
		     (remhash key rh)))
	     rl)
    (if (remhash node (nodes graph))
	node)))

(defmethod remove-from ((edge <edge>) (graph <graph> ) )
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
  (if (remhash edge (edges graph))
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
