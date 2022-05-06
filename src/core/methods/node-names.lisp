;;;; ./src/core/methods/node-names.lisp

(in-package #:mnas-graph)

(defmethod node-names ((g <graph>))
  (ids (nodes g)))
