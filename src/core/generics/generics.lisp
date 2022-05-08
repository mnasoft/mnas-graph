;;;; ./src/core/generics/generics.lisp

(in-package #:mnas-graph)

(defgeneric clear (graph)
  (:documentation
   "@b(Описание:) обобщенная функция @b(clear) возвращает граф @b(graph),
 удаляя из него все вершины и ребра.
"))

(defgeneric to-string (obj)
  (:documentation "Выполняет перобразование объекта в строку"))

(defgeneric insert-to    (obj container)
  (:documentation "Добавляет obj в container"))

(defgeneric remove-from  (obj container) (:documentation "Добавляет obj в container"))

;;;;

(defgeneric isolated-nodes (graph)
  (:documentation
   "@b(Описание:) обобщенная функция @b(inlet-nodes) возвращает
 хеш-таблицу изолированных вершин для графа @b(graph).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (let ((g (make-random-graph :node-max-number 16)))   
   (mnas-graph/view:view-graph g)
   (inlet-nodes g))
@end(code)

"))

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

(defgeneric inlet-edges (node graph)
  (:documentation
   "@b(Описание:) обобщенная функция @b(inlet-edges) возвращает
 хеш-таблицу исходящих ребер (истоков) для вершины @b(node)"))

(defgeneric outlet-edges (node graph) (:documentation " Возвращает хеш-таблицу конечных ребер (стоков)"))

(defgeneric find-node    (name graph) (:documentation "Поиск вершины по имени"))

(defgeneric find-edge    (name graph) (:documentation "Поиск ребра по имени"))

(defgeneric connected-nodes (node graph &key direction depth)
  (:documentation
   "@b(Описание:) обобщенная функция @b(connected-nodes) возвращает
 хеш-таблицу вершин при поиске в глубину начиная с вершины @b(node).

 Параметр @b(direction) задает направление поиска:
@begin(list)

 @item(:forward - поиск ведется в направлении ребер исходящих из
        вершины;)
 @item(:backward - поиск ведется в направлении ребер входящих в
        вершину;)
 @item(:both - поиск ведется в обоих направлениях.)
@end(list)

 Параметр @b(depth) задает предельную глубину поиска.
"))

(defgeneric find-forward-nodes (node graph)
  (:documentation
   "@b(Описание:) обобщенная функция @b(find-forward-nodes) возвращает
 хеш-таблицу ближайших вершин, с которыми соединена вершина @b(node), в
 направлении течения."))

(defgeneric find-backward-nodes  (node graph)
  (:documentation
   "@b(Описание:) обобщенная функция @b(find-backward-nodes) возвращает
 хеш-таблицу ближайших вершин, с которыми соединена вершина @b(node),
 в направлении против течения."))

(defgeneric find-both-nodes  (node graph)
  (:documentation
   "@b(Описание:) обобщенная функция @b(find-backward-nodes) возвращает
 хеш-таблицу ближайших вершин, с которыми соединена вершина @b(node),
 в направлениях по и против течения."))

(defgeneric name (obj)
  (:documentation
   "@b(Описание:) обобщенная функция @b(name) возвращает имя объекта."))

(defgeneric edge-names (graph)
  (:documentation
   "@b(Описание:) обобщенная функция @b(edge-names) возвращает список
   имен ребер графа @b(graph)."))
