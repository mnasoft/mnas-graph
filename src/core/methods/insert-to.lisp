;;;; ./src/core/methods/insert-to.lisp

(in-package #:mnas-graph)


(defmethod insert-to ((node-name string) (graph <graph>)
                      &aux (node (make-instance '<node> :name node-name)))
  "@b(Описание:) insert-to !!!!!!
"
  (insert-to node graph))

(defmethod insert-to ((n <node>) (graph <graph>))
  "@b(Описание:) insert-to !!!!!!
"
  (setf (gethash n (nodes graph)) n
	(owner n) graph)
  n)

(defmethod insert-to ((e <edge>) (graph <graph>))
  "@b(Описание:) insert-to ((e <edge>) (graph <graph>))!!!!!!
"
  (setf (gethash e (edges graph)) e)
  (setf (owner (tail e)) graph)
  (setf (owner (head   e)) graph)
  (setf (gethash (tail e) (nodes graph)) (tail e))
  (setf (gethash (head e) (nodes graph)) (head e))
  e)
