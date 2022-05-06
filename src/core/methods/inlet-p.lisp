;;;; ./src/core/methods/inlet-p.lisp

(in-package #:mnas-graph)

(defmethod inlet-p ((node <node>) (graph <graph>))
  (let ((inlet  (hash-table-count (find-backward-nodes node graph)))
        (outlet (hash-table-count (find-forward-nodes  node graph))))
    (when (and (< 0 inlet) (= 0 outlet))
      t)))
