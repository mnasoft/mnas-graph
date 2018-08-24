;;;; testing.lisp

(in-package #:mnas-graph)

(view-graph 
 (generate-graph
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
