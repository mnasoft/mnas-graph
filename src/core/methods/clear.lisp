;;;; ./src/core/methods/clear.lisp

(in-package #:mnas-graph)

(defmethod clear ((g <graph>))
  "@b(Описание:) clear ((g <graph>))!!!!!!
"
  (clrhash (nodes g))
  (clrhash (edges g))
  g)
