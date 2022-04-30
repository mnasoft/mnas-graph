;;;; ./src/core/methods/remove-from.lisp

(in-package #:mnas-graph)

(defmethod remove-from ((n <node>) (g <graph> ))
  "@b(Описание:) remove-from ((n <node>) (g <graph> ))!!!!!!
"
  (let* ((rh (edges g))
	 (rl (hash-table-copy rh)))
    (maphash #'(lambda(key val)
		 val
		 (if (or
		      (eq (beg-node key) n)
		      (eq (end-node key)   n))
		     (remhash key rh)))
	     rl)
    (if (remhash n (nodes g))
	n)))

(defmethod remove-from ((e <edge>) (g <graph> ) )
  "@b(Описание:) remove-from ((e <edge>) (g <graph> ) )!!!!!!
"
  (if (remhash e (edges g))
      e))
