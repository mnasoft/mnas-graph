;;;; package.lisp

(defpackage #:mnas-graph)

(defpackage #:mnas-graph
  (:use #:cl #:mnas-hash-table)
;;;; mnas-graph.lisp  
  (:intern mnas-graph::graphviz-prg
	   )
  (:export mnas-graph::to-string
	   mnas-graph::insert-to
	   mnas-graph::remove-from
	   
	   mnas-graph::inlet-nodes
	   mnas-graph::outlet-nodes
	   
	   mnas-graph::outlet-edges
	   mnas-graph::inlet-edges
	   
	   mnas-graph::find-node
	   mnas-graph::find-edge
	   
	   mnas-graph::to-graphviz
	   mnas-graph::view-graph
	   
	   mnas-graph::node
	   mnas-graph::node-name
	   mnas-graph::node-counter
	   
	   mnas-graph::edge
	   mnas-graph::edge-from
	   mnas-graph::edge-to
	   mnas-graph::graph
	   mnas-graph::graph-nodes
	   mnas-graph::graph-edges
	   )
  (:export mnas-graph::*filter-dot*
	   mnas-graph::*filter-neato*
	   mnas-graph::*filter-twopi*
	   mnas-graph::*filter-circo*
	   mnas-graph::*filter-fdp*
	   mnas-graph::*filter-sfdp*
	   mnas-graph::*filter-patchwork*
	   )
  (:export mnas-graph::*output-path*
	   mnas-graph::*viewer-path*
	   mnas-graph::make-graph
	   mnas-graph::make-random-graph
	   )
  (:export mnas-graph::demo-1
	   mnas-graph::demo-2
	   mnas-graph::demo-3
	   mnas-graph::demo-4
	   mnas-graph::demo-5 ))

;;;; (declaim (optimize (compilation-speed 0) (debug 3) (safety 0) (space 0) (speed 0)))
