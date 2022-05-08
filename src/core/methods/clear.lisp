;;;; ./src/core/methods/clear.lisp

(in-package #:mnas-graph)

(defmethod clear ((graph <graph>))
  "
 @b(Пример использования:)
@begin[lang=lisp](code)
  (let ((graph (mnas-graph:make-random-graph)))
    (format t \"~A~%~%~A\" graph (mnas-graph:clear graph)))
@end(code)
"
  (clrhash (nodes graph))
  (clrhash (edges graph))
  graph)
