;;;; ./src/core/methods/name.lisp

(in-package #:mnas-graph)

(defmethod name ((edge <edge>))
  (to-string edge))
