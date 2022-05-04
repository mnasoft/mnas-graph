;;;; tests/main.lisp

(in-package :mnas-graph/tests)

(def-suite main
  :description "Мастер-набор всех тестов проекта mnas-graph."
  :in all)

(in-suite main)

(def-test section-variables ()
  (is-true (= 5 5))
  )

(defparameter *g*
    (mnas-graph:make-graph '(("a" "c") ("b" "c") ("c" "d")
                             ("c" "g") ("c" "e") ("e" "f")
                             ("e" "g") ("h" "j") ("b" "f"))
                           :nodes
                           '("k")))



(mnas-graph:to-string
(mnas-graph:insert-to   "m" *g*) 
(mnas-graph:remove-from "e" *g*)  
(mnas-graph:inlet-nodes
(mnas-graph:outlet-nodes

(ql:quickload :mnas-hash-table)

inlet-edges
outlet-edges
find-node
find-edge
connected-nodes
(mnas-graph:find-inlet-nodes (mnas-graph:find-node *g* "c"))
(mnas-graph:find-outlet-nodes (mnas-graph:find-node *g* "c"))


(mnas-graph/view:view-graph *g*)
