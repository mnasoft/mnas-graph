;;;; ./src/core/methods/rest.lisp

(in-package #:mnas-graph)

(defmethod name ((edge <edge>))
  (to-string edge))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export 'ids)
(defmethod ids ((ht hash-table) &key (sort t) (predicate #'string<))
  (let ((ids nil))
    (maphash
     #'(lambda (key val)
         (declare (ignore key))
         (push (name val) ids))
     ht)
    (if sort
        (sort ids predicate)
        ids)))

(defmethod edge-names ((g <graph>))
  (ids (edges g)))

(defmethod node-names ((g <graph>))
  (ids (nodes g)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; to-nodes

(defmethod to-nodes  ((node string) (graph <graph>))
  (find-outlet-nodes (find-node graph node)))

(defmethod to-nodes  ((node <node>) (graph <graph>))
  (when (eq (owner node) graph)
    (find-outlet-nodes node)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; from-nodes

(defmethod from-nodes  ((node string) (graph <graph>))
  (find-inlet-nodes (find-node graph node)))

(defmethod from-nodes  ((node <node>) (graph <graph>))
  (when (eq (owner node) graph)
    (find-inlet-nodes node)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; inlet-nodes

(defmethod inlet-nodes ((graph <graph>) &aux (ht (make-hash-table)))
  (maphash
   #'(lambda (key val)
       (when (inlet-p key)
         (setf (gethash key ht) key)))
   (nodes graph))
  ht)

(defmethod outlet-nodes ((graph <graph>) &aux (ht (make-hash-table)))
  (maphash
   #'(lambda (key val)
       (when (outlet-p key)
         (setf (gethash key ht) key)))
   (nodes graph))
  ht)

(defmethod isolated-nodes ((graph <graph>) &aux (ht (make-hash-table)))
  (maphash
   #'(lambda (key val)
       (when (isolated-p key)
         (setf (gethash key ht) key)))
   (nodes graph))
  ht)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; find-inlet-nodes

(defmethod find-inlet-nodes  ((n <node>) &aux (ht (make-hash-table)))
  "@b(Описание:) метод @b(find-inlet-nodes) возвращает хеш-таблицу
   вершин, с которыми соединена вершина node, в направлении от них к
   ней."
  (maphash
   #'(lambda (key val)
       val
       (setf (gethash (tail key) ht) (tail key)))
   (inlet-edges n))
  ht)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; find-outlet-nodes

(defmethod find-outlet-nodes  ((node <node>) &aux (ht (make-hash-table)))
  "@b(Описание:) метод @b(find-outlet-nodes) возвращает хеш-таблицу вершин, с
 которыми соединена вершина <node>, в направлении от нее к ним."
  (maphash
   #'(lambda (key val)
       val
       (setf (gethash (head key) ht) (head key)))
   (outlet-edges node))
  ht)

(export 'find-both-nodes)
(defmethod find-both-nodes  ((node <node>) &aux (ht (make-hash-table)))
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

;;;;

(export 'isolated-p)

(defmethod isolated-p ((node <node>) )
  (let ((inlet  (hash-table-count (find-inlet-nodes node)))
        (outlet (hash-table-count (find-outlet-nodes node))))
    (when (and (= 0 inlet) (= 0 outlet))
      t)))

(export 'inlet-p)

(defmethod inlet-p ((node <node>) )
  (let ((inlet  (hash-table-count (find-inlet-nodes node)))
        (outlet (hash-table-count (find-outlet-nodes node))))
    (when (and (< 0 inlet) (= 0 outlet))
      t)))

(export 'outlet-p)

(defmethod outlet-p ((node <node>) )
  (let ((inlet  (hash-table-count (find-inlet-nodes node)))
        (outlet (hash-table-count (find-outlet-nodes node))))
    (when (and (< 0 outlet) (= 0 inlet))
      t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; connected-nodes

(defmethod connected-nodes ((n <node>)
                            &key (direction :both) ; :forward :backward :both
                            &aux (ht (make-hash-table ))) 
  "@b(Описание:) обобщенная функция @b(connected-nodes) возвращает
 хеш-таблицу доситжимых вершин при поиске в глубину начиная с вершины
 @b(node).
 Параметр @b(direction) задает направление поиска:
@begin(list)
 @item(:direction-to - поиск ведется в направлении ребер входящих в
        вершину;)
 @item(:direction-ftom - поиск ведется в направлении ребер исходящих
        из вершины.)
@end(list)
"
  (setf (gethash n ht) n)
  (do ((count-before -1) (count-after  0))
      ((= count-before count-after) ht)
    (setf count-before (hash-table-count ht))
    (when (eq direction :forward)
      (maphash #'(lambda (key val)
		   val
		   (maphash
		    #'(lambda (key val)
			val
			(setf (gethash  key ht) key))
		    (find-outlet-nodes key)))
	       ht))
    (when (eq direction :backward)
      (maphash #'(lambda (key val)
		   val
		   (maphash
		    #'(lambda (key val)
			val
			(setf (gethash  key ht) key))
		    (find-inlet-nodes key)))
	       ht))
    (setf count-after (hash-table-count ht))))
