;;;; ./src/core/methods/isolated-nodes.lisp

(in-package #:mnas-graph)

(defmethod isolated-nodes ((graph <graph>) &aux (ht (make-hash-table)))
  (maphash
   #'(lambda (key val)
       (when (isolated-p key graph)
         (setf (gethash key ht) key)))
   (nodes graph))
  ht)
