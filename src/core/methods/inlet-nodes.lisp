;;;; ./src/core/methods/inlet-nodes.lisp

(in-package #:mnas-graph)

(defmethod inlet-nodes ((graph <graph>) &aux (ht (make-hash-table)))
  (maphash
   #'(lambda (key val)
       (when (inlet-p key graph)
         (setf (gethash key ht) key)))
   (nodes graph))
  ht)
