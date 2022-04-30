;;;; ./src/core/methods/insert-to.lisp

(in-package #:mnas-graph)

(defmethod insert-to ((n <node>) (g <graph>))
  "@b(Описание:) insert-to !!!!!!
"
  (setf (gethash n (nodes g)) n
	(owner n) g)
  n)

(defmethod insert-to ((e <edge>) (g <graph>))
  "@b(Описание:) insert-to ((e <edge>) (g <graph>))!!!!!!
"
  (setf (gethash e (edges g)) e)
  (setf (owner (beg-node e)) g)
  (setf (owner (end-node   e)) g)
  (setf (gethash (beg-node e) (nodes g)) (beg-node e))
  (setf (gethash (end-node   e) (nodes g)) (end-node   e))
  e)
