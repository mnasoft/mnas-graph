;;;; ./src/core/methods/edge-names.lisp

(in-package #:mnas-graph)

(defmethod edge-names ((g <graph>))
    "
 @b(Пример использования:)
@begin[lang=lisp](code)
  (let ((graph (mnas-graph:make-random-graph)))
    (mnas-graph:edge-names graph))
@end(code)
"
  (ids (edges g)))
