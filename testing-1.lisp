;;;; testing.lisp

(in-package #:mnas-graph)

(view-graph
 (generate-graph
  (mapcar #'(lambda (el) (list (second el) (first el)))
  '(
    ("asdf-files" "ROOT")
    ("read-file" "ROOT")
    ("callers" "orphan-p")
    ("callers" "call-graph")
    ("ensure-package-list" "call-graph")
    ("ensure-package-list" "map-symbols")
    ("map-symbols" "call-graph")
    ("map-symbols" "check-for-orphans")
    ("orphan-p" "check-for-orphans")
    ("check-for-orphans" "ROOT")
    ("cluster-printer" "print-object")
    ("node-printer" "print-object")
    ("call-graph" "function-call-graph")
    ("call-graph" "call-graph->dot")
    ("call-graph" "function-caller-graph")
    ("graphviz-escape" "->dot")
    ("->dot" "call-graph->dot")
    ("->dot" "save-as-svg")
    ("save-as-svg" "ROOT")
    ("call-graph->dot" "ROOT")
    ("function-call-graph" "ROOT")
    ("function-caller-graph" "ROOT")
    ("asdf-systems" "asdf-who-depends")
    ("asdf-systems" "asdf-graph")
    ("*syscache*" "asdf-dependencies")
    ("asdf-depends-on-p" "asdf-who-depends")
    ("asdf-who-depends" "ROOT")
    ("keyword-name" "ROOT")
    ("%asdf-graph" "%asdf-graph")
    ("%asdf-graph" "asdf-graph")
    ("asdf-graph" "ROOT")))))



