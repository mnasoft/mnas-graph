;;;; ./src/core/methods/clear.lisp

(in-package #:mnas-graph)

(defmethod clear ((graph <graph>))
  (clrhash (nodes graph))
  (clrhash (edges graph))
  graph)
