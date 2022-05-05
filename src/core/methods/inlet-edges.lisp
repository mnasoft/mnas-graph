;;;; ./src/core/methods/inlet-edges.lisp

(in-package #:mnas-graph)

(defmethod inlet-edges ((node <node>) &aux (g (owner node)))
  "@b(Описание:) inlet-edges ((node <node>) &aux (g (owner node)))!!!!!!
"
  (let ((rez-tbl (mnas-hash-table:hash-table-copy (edges g))))
    (maphash
     #'(lambda (key val)
	 val
	 (if (not(eq (head key) node))
	     (remhash  key rez-tbl)))
     (edges g))
    rez-tbl))
