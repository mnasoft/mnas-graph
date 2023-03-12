;;;; ./src/core/methods/count-nodes.lisp

(in-package :mnas-graph)

(defmethod count-nodes ((graph <graph>))
  " @b(Описание:) метод @b(count-nodes) возвращает количество вершин
графа @b(graph).

 @b(Пример использования:)
@begin[lang=lisp](code);
 (count-nodes (make-random-graph)) -> some wat less then 100
@end(code)
"
  (hash-table-count (nodes graph)))
