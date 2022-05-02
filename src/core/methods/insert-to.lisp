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
  (setf (owner (tail e)) g)
  (setf (owner (head   e)) g)
  (setf (gethash (tail e) (nodes g)) (tail e))
  (setf (gethash (head e) (nodes g)) (head e))
  e)
