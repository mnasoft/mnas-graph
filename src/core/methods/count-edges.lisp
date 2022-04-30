;;;; ./src/core/methods/count-edges.lisp

(in-package #:mnas-graph)

(defmethod count-edges ((graph <graph>))
  " @b(Описание:) метод @b(count-edges) возвращает количество ребер
 графа @b(graph).

 @b(Пример использования:)
@begin[lang=lisp](code);
 (count-edges (make-random-graph)) -> 100
@end(code)
"
  (hash-table-count (edges graph)))
