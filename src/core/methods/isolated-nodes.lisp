;;;; ./src/core/methods/isolated-nodes.lisp

(in-package :mnas-graph)

(defmethod isolated-nodes ((graph <graph>) &aux (ht (make-hash-table)))
  "
 @b(Пример использования:)
@begin[lang=lisp](code)
 (let ((graph (make-random-graph :node-max-number 16)))   
   (mnas-graph/view:view-graph graph)
   (mnas-graph:ids (mnas-graph:isolated-nodes graph)))
@end(code)
"
  (maphash
   #'(lambda (key val)
       val
       (when (isolated-p key graph)
         (setf (gethash key ht) key)))
   (nodes graph))
  ht)
