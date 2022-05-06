;;;; ./src/core/methods/find-both-nodes.lisp

(in-package #:mnas-graph)

(defmethod find-both-nodes  ((node <node>) (graph <graph>) &aux (ht (make-hash-table)))
  "@b(Описание:) метод @b(find-both-nodes) возвращает хеш-таблицу вершин, с
 которыми соединена вершина <node>, в направлении от нее к ним."
  (maphash
   #'(lambda (key val)
       val
       (setf (gethash (head key) ht) (head key))
       (setf (gethash (tail key) ht) (tail key))
       (remhash node ht))
   (both-edges node))
  ht)
