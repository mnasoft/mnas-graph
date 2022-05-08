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
