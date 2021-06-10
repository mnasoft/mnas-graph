;;;; ./src/filter/filter.lisp

(defpackage #:mnas-graph/filter
  (:use #:cl)
  (:export *filter-dot*
           *filter-neato*
           *filter-circo*
           *filter-fdp*
           *filter-sfdp*
           *filter-twopi*
           *filter-patchwork*))

(in-package  #:mnas-graph/filter)

(defun make-filter-prg-path (program)
  (cond
    ((and (uiop/os:os-windows-p)
	  (uiop:getenv "MSYSTEM_PREFIX"))
     (concatenate 'string (uiop:getenv "MSYSTEM_PREFIX") "/" "bin" "/" program "." "exe"))
    ((uiop/os:os-unix-p)
     (concatenate 'string "/usr/bin/" program))))

(defparameter *filter-dot* (make-filter-prg-path "dot")
  "dot       - filter for drawing directed graphs"
  )

(defparameter *filter-neato* (make-filter-prg-path "neato")
  "neato     - filter for drawing undirected graphs"
  )

(defparameter *filter-twopi* (make-filter-prg-path "twopi")
  "twopi     - filter for radial layouts of graphs"
  )

(defparameter *filter-circo* (make-filter-prg-path "circo")
  "circo     - filter for circular layout of graphs"
  )

(defparameter *filter-fdp* (make-filter-prg-path "fdp")
  "fdp       - filter for drawing undirected graphs"
  )

(defparameter *filter-sfdp* (make-filter-prg-path "sfdp")
  "sfdp      - filter for drawing large undirected graphs"
  )

(defparameter *filter-patchwork* (make-filter-prg-path "patchwork")
  "patchwork - filter for tree maps"
  )
