;;;; ./src/core/methods/ids.lisp

(in-package #:mnas-graph)

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
