;;;; ./src/core/methods/outlet-nodes.lisp

(in-package #:mnas-graph)

(defmethod outlet-nodes ((graph <graph>) &aux (ht (make-hash-table)))
    "
 @b(Пример использования:)
@begin[lang=lisp](code)
 (let ((graph (make-random-graph :node-max-number 16)))   
   (mnas-graph/view:view-graph graph)
   (outlet-nodes graph))
@end(code)
"
  (maphash
   #'(lambda (key val)
       (when (outlet-p key graph)
         (setf (gethash key ht) key)))
   (nodes graph))
  ht)

