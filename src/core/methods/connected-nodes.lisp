;;;; ./src/core/methods/connected-nodes.lisp

(in-package :mnas-graph)

(defmethod connected-nodes ((node <node>) (graph <graph>)
                            &key
                              (direction :both) ;; :forward :backward :both
                              (depth `,(1- (expt 2 62))) ;; глубина поиска не более
                            &aux (ht (make-hash-table ))) 
"
 @b(Пример использования:)
@begin[lang=lisp](code)
  (let ((graph (mnas-graph:make-graph
                '((\"a\" \"c\") (\"c\" \"d\") (\"c\" \"g\") (\"c\" \"e\")
                  (\"e\" \"f\") (\"e\" \"g\") (\"h\" \"j\") (\"b\" \"f\"))
                :nodes '(\"k\"))))
    (connected-nodes (mnas-graph:find-node \"c\" graph) graph))
@end(code)
"  
  (setf (gethash node ht) node)
  (do ((count-before -1) (count-after  0))
      ((or (= count-before count-after)
           (< (decf depth) 0))
       ht)
    (setf count-before (hash-table-count ht))
    (ecase direction
      (:forward
       (maphash #'(lambda (key val)
		    val
		    (maphash
		     #'(lambda (key val)
			 val
			 (setf (gethash  key ht) key))
		     (find-forward-nodes key graph)))
	        ht))
      (:backward
       (maphash #'(lambda (key val)
		    val
		    (maphash
		     #'(lambda (key val)
			 val
			 (setf (gethash  key ht) key))
		     (find-backward-nodes key graph)))
                ht))
      (:both
       (maphash #'(lambda (key val)
		    val
		    (maphash
		     #'(lambda (key val)
			 val
			 (setf (gethash  key ht) key))
		     (find-both-nodes key graph)))
	        ht)))
    (setf count-after (hash-table-count ht))))
