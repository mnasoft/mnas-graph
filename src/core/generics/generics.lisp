;;;; ./src/core/generics/generics.lisp

(in-package #:mnas-graph)

(defgeneric copy (obj)
  (:documentation
   "@b(Описание:) обобщенная функция @b(copy) возвращает копию объекта
 граф @b(obj).
"))

(defgeneric clear (graph)
  (:documentation
   "@b(Описание:) обобщенная функция @b(clear) возвращает граф @b(graph),
 удаляя из него все вершины и ребра.
"))

(defgeneric count-edges (graph)
  (:documentation
   "@b(Описание:) метод @b(count-edges) возвращает количество ребер
 графа @b(graph)."))

(defgeneric count-nodes (graph)
  (:documentation
   "@b(Описание:) метод @b(count-nodes) возвращает количество вершин
графа @b(graph)."))

(defgeneric edge-names (graph)
  (:documentation
   "@b(Описание:) обобщенная функция @b(edge-names) возвращает
 отсортированный список имен ребер графа @b(graph).
  "))

(defgeneric node-names (graph)
  (:documentation
   "@b(Описание:) обобщенная функция @b(edge-names) возвращает
 отсортированный список имен вуршин графа @b(graph).
  "))

(defgeneric to-string (obj)
  (:documentation
   "@b(Описание:) обобщенная функция @b(to-string) 
Выполняет перобразование объекта в строку"))

(defgeneric insert-to (obj container)
  (:documentation
   "@b(Описание:) обобщенная_функция @b(insert-to) возвращает объект
@b(obj), добавляемый в @b(container)."))

(defgeneric remove-from (obj container)
  (:documentation
   "@b(Описание:) обобщенная функция @b(remove-from) возвращает объект
 @b(obj), удаляемый из контейнера @b(container)."))

;;;;

(defgeneric isolated-nodes (graph)
  (:documentation
   "@b(Описание:) обобщенная функция @b(inlet-nodes) возвращает
 хеш-таблицу изолированных вершин для графа @b(graph).
"))

(defgeneric inlet-nodes (graph)
  (:documentation
   "@b(Описание:) обобщенная функция @b(inlet-nodes) возвращает
 хеш-таблицу вершин-стоков для графа @b(graph).
"))

(defgeneric outlet-nodes (graph)
  (:documentation
   "@b(Описание:) обобщенная функция @b(outlet-nodes) возвращает
хеш-таблицу вершин-истоков для графа @b(graph)."))

(defgeneric both-edges (node graph)
  (:documentation
   "@b(Описание:) обобщенная функция @b(inlet-edges) возвращает
 хеш-таблицу ребер, входящих в и исходящих из вершины @b(node) графа
 @b(graph)."))

(defgeneric inlet-edges (node graph)
  (:documentation
   "@b(Описание:) обобщенная функция @b(inlet-edges) возвращает
 хеш-таблицу ребер, входящих в вершину @b(node) графа @b(graph)."))

(defgeneric outlet-edges (node graph)
    (:documentation
   "@b(Описание:) обобщенная функция @b(inlet-edges) возвращает
 хеш-таблицу ребер, исходящих из вершины @b(node) графа @b(graph)."))

(defgeneric inlet-p (node graph)
  (:documentation
   "@b(Описание:) обобщенная функция @b(inlet-p) возвращает t, если
вершина @b(node) графа  @b(graph) является стоком."))

(defgeneric outlet-p (node graph)
  (:documentation
   "@b(Описание:) обобщенная функция @b(outlet-p) возвращает t, если
вершина @b(node) графа @b(graph) является источником."))

(defgeneric isolated-p (node graph)
  (:documentation
   "@b(Описание:) обобщенная функция @b(isolated-p) возвращает t, если
вершина @b(node) графа @b(graph) является изолированой."))

(defgeneric find-node (name graph)
  (:documentation
   "@b(Описание:) обобщенная функция @b(find-node) возвращает ребро с
именем @b(name), поиск которого осуществляется среди ребер графа @b(graph)."))

(defgeneric find-edge (name graph)
  (:documentation
   "@b(Описание:) обобщенная функция @b(find-edge) возвращает ребро с
именем @b(name), поиск которого осуществляется среди ребер графа @b(graph)."))

(defgeneric ids (hash-table &key sort predicate)
  (:documentation
   "@b(Описание:) обобщенная функция @b(ids) возвращает список имен
ключей, находящихся в хеш-таблице @b(hash-table). Если параметр
@(sort) равен t возвращаемый список сортируется по предикату
@b(predicate)."))

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
   "@b(Описание:) обобщенная функция @b(find-both-nodes) возвращает
 хеш-таблицу ближайших вершин, с которыми соединена вершина @b(node),
 в направлениях по и против течения."))

(defgeneric name (obj)
  (:documentation
   "@b(Описание:) обобщенная функция @b(name) возвращает имя объекта."))

(defgeneric edge-names (graph)
  (:documentation
   "@b(Описание:) обобщенная функция @b(edge-names) возвращает список
   имен ребер графа @b(graph)."))
