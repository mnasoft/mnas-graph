;;;; package.lisp

(defpackage #:mnas-graph
  (:use #:cl #:mnas-hash-table)
  (:intern graphviz-prg)
  (:export to-string
	   insert-to    remove-from
	   inlet-nodes  outlet-nodes
	   outlet-edges inlet-edges
	   find-node    find-edge
	   to-graphviz
	   view-graph
	   node         node-name    node-counter
	   edge         edge-from    edge-to
	   graph        graph-nodes  graph-edges
	   )
  (:export *filter-dot* *filter-neato* *filter-twopi* *filter-circo* *filter-fdp* *filter-sfdp* *filter-patchwork*)
  (:export
  *output-path*
  *viewer-path*
  make-graph make-random-graph)
  (:export demo-1 demo-2 demo-3 demo-4 demo-5 ))
