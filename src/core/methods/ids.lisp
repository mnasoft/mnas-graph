;;;; ./src/core/methods/ids.lisp

(in-package #:mnas-graph)

(defmethod ids ((ht hash-table) &key (sort t) (predicate #'string<))
"
 @b(Пример использования:)
@begin[lang=lisp](code)
  (let ((graph (mnas-graph:make-graph
                '((\"a\" \"c\") (\"c\" \"d\") (\"c\" \"g\") (\"c\" \"e\")
                  (\"e\" \"f\") (\"e\" \"g\") (\"h\" \"j\") (\"b\" \"f\"))
                :nodes '(\"k\"))))
    (list 
     (mnas-graph:ids (mnas-graph:nodes graph))
     (mnas-graph:ids (mnas-graph:edges graph))))
@end(code)
"  
  (let ((ids nil))
    (maphash
     #'(lambda (key val)
         (declare (ignore key))
         (push (name val) ids))
     ht)
    (if sort
        (sort ids predicate)
        ids)))

(defmethod ids ((graph <graph>) &key (sort t) (predicate #'string<))
  "@b(Описание:) метод @b(ids) возвращает список в формате согласованном
с фунцией make-graph.
"
  (let ((edges (sort 
                (mapcar #'to-list (mnas-hash-table:keys (edges graph)))
                predicate
                :key #'(lambda (el) (format nil "~S->~S" (first el) (second el )))))
        (nodes (ids (nodes graph) :sort sort :predicate predicate)))
    (list edges
          :nodes
          (set-difference nodes (apply #'append edges) :test #'equal))))

(export 'to-list)
(defmethod to-list ((edge <edge>))
  (list (name (tail edge)) (name (head edge))))


