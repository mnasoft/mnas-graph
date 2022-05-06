;;;; ./src/core/methods/find-backward-nodes.lisp

(in-package #:mnas-graph)

(defmethod find-backward-nodes (node (graph <graph>))
  (make-hash-table))

(defmethod find-backward-nodes ((node string) (graph <graph>))
  (find-backward-nodes (find-node node graph) graph))

(defmethod find-backward-nodes  ((node <node>) (graph <graph>) &aux (ht (make-hash-table)))
  "@b(Описание:) метод @b(find-backward-nodes) возвращает хеш-таблицу
   вершин, с которыми соединена вершина node, в направлении от них к
   ней."
  (maphash
   #'(lambda (key val)
       val
       (setf (gethash (tail key) ht) (tail key)))
   (inlet-edges node graph))
  ht)
