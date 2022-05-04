;;;; ./src/core/methods/rest.lisp

(in-package #:mnas-graph)

(defmethod name ((edge <edge>))
  (to-string edge))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod name-edges ((g <graph>))
  (let ((names nil))
    (maphash
     #'(lambda (key val)
         (push (name val) names))
     (edges g))
    #+nil
    (sort names #'string<)))

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
  (mnas-hash-table:print-items ht)
  ht)

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

(defmethod inlet-nodes ((graph <graph>))
  (error "(defmethod inlet-nodes ((graph <graph>)) - not yet defined.")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; find-inlet-nodes

(defmethod find-inlet-nodes  ((n <node>) &aux (ht (make-hash-table)))
  "@b(Описание:) find-inlet-nodes
Возвращает хеш-таблицу вершин, с которыми соединена вершина <node>, в направлении от них к ней.
"
  (maphash
   #'(lambda (key val)
       val
       (setf (gethash (tail key) ht) (tail key)))
   (inlet-edges n))
  (mnas-hash-table:print-items ht)
  ht)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; connected-nodes

(defmethod connected-nodes ((n <node>)
                            &key (direction :direction-to)
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
    (when (eq direction :direction-to)
      (maphash #'(lambda (key val)
		   val
		   (maphash
		    #'(lambda (key val)
			val
			(setf (gethash  key ht) key))
		    (find-outlet-nodes key)))
	       ht))
    (when (eq direction :direction-from)
      (maphash #'(lambda (key val)
		   val
		   (maphash
		    #'(lambda (key val)
			val
			(setf (gethash  key ht) key))
		    (find-inlet-nodes key)))
	       ht))
    (setf count-after (hash-table-count ht))))
