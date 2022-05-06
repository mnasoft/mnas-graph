;;;; ./src/core/methods/outlet-edges.lisp

(in-package #:mnas-graph)

(defmethod outlet-edges ((n <node>) (graph <graph>))
  "@b(Описание:) outlet-edges ((n <node>) &aux (graph (owner n)))!!!!!!
"
  (let ((rez-tbl (mnas-hash-table:hash-table-copy (edges graph))))
    (maphash
     #'(lambda (key val)
	 val
	 (if (not(eq (tail key) n))
	     (remhash  key rez-tbl)))
     (edges graph))
    rez-tbl))

(export 'both-edges)
(defmethod both-edges ((n <node>) &aux (g (owner n)))
  "@b(Описание:) both-edges ((n <node>) &aux (g (owner n)))!!!!!!
"
  (let ((rez-tbl (mnas-hash-table:hash-table-copy (edges g))))
    (maphash
     #'(lambda (key val)
	 val
	 (if (and (not (eq (tail key) n))
                  (not (eq (head key) n)))
	     (remhash  key rez-tbl)))
     (edges g))
    rez-tbl))
