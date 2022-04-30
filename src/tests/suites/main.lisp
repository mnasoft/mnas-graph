;;;; tests/main.lisp

(in-package :mnas-graph/tests)

(def-suite main
  :description "Мастер-набор всех тестов проекта mnas-graph."
  :in all)

(in-suite main)

(def-test section-variables ()
  (is-true (= 5 5))
  )

(progn 
  (defparameter *g*
    (mnas-graph:make-graph '(("a" "c") ("b" "c") ("c" "d")
                             ("c" "g") ("c" "e") ("e" "f")
                             ("e" "g") ("h" "j") ("b" "f"))
                           :nodes
                           '("k")))

  (mnas-graph/view:view-graph *g*)

  (mnas-graph:to-nodes "c" *g*)
  (mnas-graph:from-nodes "c" *g*)

  (mnas-graph:nea-from-nodes
   (mnas-graph:find-node  *g* "c"))
  (mnas-graph:nea-to-nodes
   (mnas-graph:find-node  *g* "c"))
  (mnas-graph:connected-nodes
   (mnas-graph:find-node  *g* "a")))

(def-test section-variables ()
  (is-true (= 5 5))
  )
(mnas-graph:<node>-counter
