;;;; tests/main.lisp

(in-package :mnas-graph/tests)

(def-suite main
  :description "Мастер-набор всех тестов проекта mnas-graph."
  :in all)

(in-suite main)

(def-test section-variables ()
  (is-true (= 5 5))
  )

(defparameter *g*   (make-graph '(("a" "c") ("b" "c") ("c" "d")
                                  ("c" "g") ("c" "e") ("e" "f")
                                  ("e" "g") ("h" "j") ("b" "f"))
                                :nodes
                                '("k")))

(mnas-graph/view:view-graph *g*)

(to-nodes "c" *g*)
(from-nodes "c" *g*)

(nea-from-nodes (find-node  *g* "c"))
(nea-to-nodes (find-node  *g* "c"))

(connected-nodes (find-node  *g* "a"))
