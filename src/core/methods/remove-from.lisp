;;;; ./src/core/methods/remove-from.lisp

(in-package #:mnas-graph)

(defmethod remove-from ((node-name string) (g <graph>)
                        &aux (node (find-node g node-name)))
  "@b(Описание:) remove-from ((node <node>) (g <graph> ))!!!!!!
"
  (remove-from node g))

(defmethod remove-from ((node <node>) (g <graph> ))
  "@b(Описание:) remove-from ((node <node>) (g <graph> ))!!!!!!
"
  (let* ((rh (edges g))
	 (rl (mnas-hash-table:hash-table-copy rh)))
    (maphash #'(lambda(key val)
		 val
		 (if (or
		      (eq (tail key) node)
		      (eq (head key)   node))
		     (remhash key rh)))
	     rl)
    (if (remhash node (nodes g))
	node)))

(defmethod remove-from ((e <edge>) (g <graph> ) )
  "@b(Описание:) remove-from ((e <edge>) (g <graph> ) )!!!!!!
"
  (if (remhash e (edges g))
      e))
