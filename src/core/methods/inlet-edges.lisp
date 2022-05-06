;;;; ./src/core/methods/inlet-edges.lisp

(in-package #:mnas-graph)

(defmethod inlet-edges ((node <node>) (graph <graph>))
  "@b(Описание:) inlet-edges ((node <node>) &aux (graph (owner node)))!!!!!!
"
  (let ((rez-tbl (mnas-hash-table:hash-table-copy (edges graph))))
    (maphash
     #'(lambda (key val)
	 val
	 (if (not(eq (head key) node))
	     (remhash  key rez-tbl)))
     (edges graph))
    rez-tbl))
