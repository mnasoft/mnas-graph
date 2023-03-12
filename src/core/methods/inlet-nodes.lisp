;;;; ./src/core/methods/inlet-nodes.lisp

(in-package :mnas-graph)

(defmethod inlet-nodes ((graph <graph>) &aux (ht (make-hash-table)))
  "
 @b(Пример использования:)
@begin[lang=lisp](code)
 (let ((graph (make-random-graph :node-max-number 16)))   
   (mnas-graph/view:view-graph graph)
   (inlet-nodes graph))
@end(code)
"
  (maphash
   #'(lambda (key val)
       (when (inlet-p key graph)
         (setf (gethash key ht) key)))
   (nodes graph))
  ht)
