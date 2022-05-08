;;;; ./src/core/methods/inlet-edges.lisp

(in-package #:mnas-graph)

(defmethod inlet-edges ((node <node>) (graph <graph>))
"
 @b(Пример использования:)
@begin[lang=lisp](code)
  (let ((graph (mnas-graph:make-graph
                '((\"a\" \"c\") (\"c\" \"d\") (\"c\" \"g\") (\"c\" \"e\")
                  (\"e\" \"f\") (\"e\" \"g\") (\"h\" \"j\") (\"b\" \"f\"))
                :nodes '(\"k\"))))
    (mnas-graph:inlet-edges (mnas-graph:find-node \"c\" graph) graph))
@end(code)
"  
  (let ((rez-tbl (mnas-hash-table:hash-table-copy (edges graph))))
    (maphash
     #'(lambda (key val)
	 val
	 (if (not(eq (head key) node))
	     (remhash  key rez-tbl)))
     (edges graph))
    rez-tbl))
