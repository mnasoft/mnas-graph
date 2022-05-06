;;;; ./src/core/methods/connected-nodes.lisp

(in-package #:mnas-graph)

(defmethod connected-nodes ((n <node>) (graph <graph>)
                            &key
                              (direction :both) ;; :forward :backward :both
                              (depth `,(1- (expt 2 62))) ;; глубина поиска не более
                            &aux (ht (make-hash-table ))) 
  "@b(Описание:) обобщенная функция @b(connected-nodes) возвращает
 хеш-таблицу вершин при поиске в глубину начиная с вершины @b(node).

 Параметр @b(direction) задает направление поиска:
@begin(list)

 @item(:forward - поиск ведется в направлении ребер исходящих из
        вершины;)
 @item(:backward - поиск ведется в направлении ребер входящих в
        вершину;)
 @item(:both - поиск ведется в обоих направлениях.)
@end(list)

 Параметр @b(depth) задает предельную глубину поиска.
"
  (setf (gethash n ht) n)
  (do ((count-before -1) (count-after  0))
      ((or (= count-before count-after)
           (< (decf depth) 0))
       ht)
    (setf count-before (hash-table-count ht))
    (ecase direction
      (:forward
       (maphash #'(lambda (key val)
		    val
		    (maphash
		     #'(lambda (key val)
			 val
			 (setf (gethash  key ht) key))
		     (find-forward-nodes key graph)))
	        ht))
      (:backward
       (maphash #'(lambda (key val)
		    val
		    (maphash
		     #'(lambda (key val)
			 val
			 (setf (gethash  key ht) key))
		     (find-backward-nodes key graph)))
                ht))
      (:both
       (maphash #'(lambda (key val)
		    val
		    (maphash
		     #'(lambda (key val)
			 val
			 (setf (gethash  key ht) key))
		     (find-both-nodes key graph)))
	        ht)))
    (setf count-after (hash-table-count ht))))
