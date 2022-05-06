;;;; ./src/core/methods/edge-names.lisp

(in-package #:mnas-graph)

(defmethod edge-names ((g <graph>))
  (ids (edges g)))

