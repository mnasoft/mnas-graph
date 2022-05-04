;;;; ./src/core/methods/inlet-edges.lisp

(in-package #:mnas-graph)

(defmethod inlet-edges ((n <node>) &aux (g (owner n)))
  "@b(Описание:) inlet-edges ((n <node>) &aux (g (owner n)))!!!!!!
"
  (let ((rez-tbl (mnas-hash-table:hash-table-copy (edges g))))
    (maphash
     #'(lambda (key val)
	 val
	 (if (not(eq (head key) n))
	     (remhash  key rez-tbl)))
     (edges g))
    rez-tbl))
