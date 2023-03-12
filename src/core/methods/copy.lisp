;;;; ./src/core/methods/copy.lisp

(in-package :mnas-graph)

#+nil
(defmethod copy ((node <node>))
  (make-instance '<node> :name (name node)))

#+nil
(defmethod copy ((edge <edge>))
  (make-instance '<edge>
                 :tail (copy (tail edge))
                 :head (copy (head edge))))

(defmethod copy ((graph <graph>))
  (apply #'make-graph (ids graph)))

