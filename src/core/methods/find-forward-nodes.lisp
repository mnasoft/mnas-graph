;;;; ./src/core/methods/find-forward-nodes.lisp

(in-package #:mnas-graph)

(defmethod find-forward-nodes (node (graph <graph>))
  (make-hash-table))

(defmethod find-forward-nodes ((node string) (graph <graph>))
  (find-forward-nodes (find-node node graph) graph))

(defmethod find-forward-nodes  ((node <node>) (graph <graph>) &aux (ht (make-hash-table)))
  "@b(Описание:) метод @b(find-forward-nodes) возвращает хеш-таблицу вершин, с
 которыми соединена вершина <node>, в направлении от нее к ним."
  (maphash
   #'(lambda (key val)
       val
       (setf (gethash (head key) ht) (head key)))
   (outlet-edges node graph))
  ht)
