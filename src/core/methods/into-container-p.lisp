;;;; ./src/core/methods/into-container-p.lisp

(in-package #:mnas-graph)

(defmethod into-container-p ((node <node>) (graph <graph>))
  (nth-value 1 (gethash node (nodes graph))))

(defmethod into-container-p ((edge <edge>) (graph <graph>))
  (nth-value 1 (gethash edge (edges graph))))



