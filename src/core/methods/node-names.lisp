;;;; ./src/core/methods/node-names.lisp

(in-package :mnas-graph)

(defmethod node-names ((g <graph>))
  "
 @b(Пример использования:)
@begin[lang=lisp](code)
  (let ((graph (mnas-graph:make-random-graph)))
    (mnas-graph:node-names graph))
@end(code)
"
  (ids (nodes g)))
