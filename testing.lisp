;;;; testing.lisp

(in-package #:mnas-graph)

(progn
  (defparameter *g* (make-instance 'graph ))
  (defparameter *v1* (make-instance 'vertex :vertex-node "A"))
  (defparameter *v2* (make-instance 'vertex :vertex-node "B"))
  (graph-add-vertex *g* *v1*)
  (graph-add-vertex *g* *v2*)
  (graph-add-rib  *g*
		  (make-instance 'rib
				 :rib-start-vertex
				 (graph-find-vertex-by-name *g* "A:2")
				 :rib-end-vertex
				 (graph-find-vertex-by-name *g* "B:3")) )
  )




(to-string *v2*)

*g*
