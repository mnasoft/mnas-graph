;;;; ./src/core/methods/outlet-p.lisp

(in-package #:mnas-graph)

(defmethod outlet-p ((node <node>) (graph <graph>))
  (let ((inlet  (hash-table-count (find-backward-nodes node graph)))
        (outlet (hash-table-count (find-forward-nodes  node graph))))
    (when (and (< 0 outlet) (= 0 inlet))
      t)))
