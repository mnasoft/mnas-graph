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
;;;; nea-to-nodes

(defmethod nea-to-nodes  ((n <node>) &aux (ht (make-hash-table)))
  "@b(Описание:) метод @b(nea-to-nodes) возвращает хеш-таблицу вершин, с
 которыми соединена вершина <node>, в направлении от нее к ним."
  (maphash
   #'(lambda (key val)
       val
       (setf (gethash (end-node key) ht) (end-node key)))
   (outlet-edges n))
  (print-items ht)
  ht)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; to-nodes

(defmethod to-nodes  ((node string) (g <graph>))
  (nea-to-nodes (find-node g node)))

(defmethod to-nodes  ((node <node>) (g <graph>))
  (when (eq (owner node) g)
    (nea-to-nodes node)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; from-nodes

(defmethod from-nodes  ((node string) (g <graph>))
  (nea-from-nodes (find-node g node)))

(defmethod from-nodes  ((node <node>) (g <graph>))
  (when (eq (owner node) g)
    (nea-from-nodes node)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; nea-from-nodes

(defmethod nea-from-nodes  ((n <node>) &aux (ht (make-hash-table)))
  "@b(Описание:) nea-from-nodes
Возвращает хеш-таблицу вершин, с которыми соединена вершина <node>, в направлении от них к ней.
"
  (maphash
   #'(lambda (key val)
       val
       (setf (gethash (beg-node key) ht) (beg-node key)))
   (inlet-edges n))
  (print-items ht)
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
		    (nea-to-nodes key)))
	       ht))
    (when (eq direction :direction-from)
      (maphash #'(lambda (key val)
		   val
		   (maphash
		    #'(lambda (key val)
			val
			(setf (gethash  key ht) key))
		    (nea-from-nodes key)))
	       ht))
    (setf count-after (hash-table-count ht))))
