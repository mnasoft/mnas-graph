;;;; testing.lisp

(in-package #:mnas-graph)

(view-graph 
 (make-graph
  '(("a" "b")
    ("a" "c")
    ("a" "d")
    ("d" "ф")
    ("d" "й")))
 :graphviz-prg :filter-neato)

(view-graph
 (make-graph
  '(("a" "b")
    ("a" "c")
    ("a" "d")
    ("d" "ф")
    ("d" "й"))
  :nodes
  '("я1" "α1")))

(make-instance '<node> :name "A")

(progn
  (defparameter *g* (make-random-graph :node-max-number 100))
  (view-graph *g* :graphviz-prg :filter-neato)
  (connected-nodes (find-node *g* "57") :direction :direction-to))
