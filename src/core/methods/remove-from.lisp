;;;; ./src/core/methods/remove-from.lisp

(in-package #:mnas-graph)

(defmethod remove-from ((node-name string) (graph <graph>)
                        &aux (node (find-node node-name graph)))
  "@b(Описание:) remove-from ((node <node>) (graph <graph> ))!!!!!!
"
  (remove-from node graph))

(defmethod remove-from ((node <node>) (graph <graph> ))
  "@b(Описание:) remove-from ((node <node>) (graph <graph> ))!!!!!!
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
  "@b(Описание:) remove-from ((edge <edge>) (graph <graph> ) )!!!!!!
"
  (if (remhash edge (edges graph))
      edge))
