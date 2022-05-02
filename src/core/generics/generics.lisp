;;;; ./src/core/generics/generics.lisp

(in-package #:mnas-graph)

(defgeneric to-string    (obj)
  (:documentation "Выполняет перобразование объекта в строку"))

(defgeneric insert-to    (obj container)
  (:documentation "Добавляет obj в container"))

(defgeneric remove-from  (obj container) (:documentation "Добавляет obj в container"))

(defgeneric inlet-nodes (graph)
  (:documentation
   "@b(Описание:) обобщенная функция @b(inlet-nodes) возвращает
 хеш-таблицу вершин-стоков для графа @b(graph).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (let ((g (make-random-graph :node-max-number 16)))   
   (mnas-graph/view:view-graph g)
   (inlet-nodes g))
@end(code)

"))

(defgeneric outlet-nodes (graph)
  (:documentation
   "@b(Описание:) обобщенная функция @b(outlet-nodes) возвращает
хеш-таблицу вершин-истоков для графа @b(graph)."))

(defgeneric inlet-edges (node)
  (:documentation
   "@b(Описание:) обобщенная функция @b(inlet-edges) возвращает
 хеш-таблицу исходящих ребер (истоков) для вершины @b(node)"))

(defgeneric outlet-edges (node) (:documentation " Возвращает хеш-таблицу конечных ребер (стоков)"))

(defgeneric find-node    (graph name) (:documentation "Поиск вершины по имени"))

(defgeneric find-edge    (graph edge-name) (:documentation "Поиск ребра по имени"))

(defgeneric connected-nodes (node &key direction)
  (:documentation
   " @b(Описание:) обобщенная функция @b(connected-nodes) возвращает
 хеш-таблицу доситжимых вершин при поиске в глубину начиная с вершины
 @b(node).
 Параметр @b(direction) задает направление поиска:
@begin(list)
 @item(:direction-to - поиск ведется в направлении ребер входящих в
        вершину;)
 @item(:direction-ftom - поиск ведется в направлении ребер исходящих
        из вершины.)
@end(list)
"))

(defgeneric nea-from-nodes  (node)
  (:documentation
   "@b(Описание:) обобщенная функция @b(nea-from-nodes) возвращает
 хеш-таблицу вершин, с которыми соединена вершина @b(node), в
 направлении от нее к ним."))

(defgeneric nea-to-nodes    (node)
  (:documentation
   "@b(Описание:) обобщенная функция @b(nea-to-nodes) возвращает
 хеш-таблицу вершин, с которыми соединена вершина @b(node), в
 направлении от них к ней"))
