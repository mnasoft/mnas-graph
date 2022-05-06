;;;; ./src/core/methods/outlet-nodes.lisp

(in-package #:mnas-graph)

(defmethod outlet-nodes ((graph <graph>) &aux (ht (make-hash-table)))
  (maphash
   #'(lambda (key val)
       (when (outlet-p key graph)
         (setf (gethash key ht) key)))
   (nodes graph))
  ht)

