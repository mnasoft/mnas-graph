;;;; ./src/core/classes/edge.lisp

(in-package #:mnas-graph)

(defclass <edge> (<edge-attributes>)
  ((tail
    :accessor tail
    :initarg :tail
    :initform nil
    :documentation
    "Начальная вершина ребра")
   (head
    :accessor head
    :initarg :head
    :initform nil
    :documentation
    "Конечная  вершина ребра"))
  (:documentation "@b(Описание:) класс @b(<edge>) представляет ребро графа.
                                                                                "))
(defmethod print-object ((x <edge>) s)
    (format s "~S->~S"
          (name (tail x))
          (name (head x))))

#+nil(defmethod print-object :after ((x <edge>) s))

(defmethod initialize-instance :after ((edge <edge>) &key (owner))
  (assert (or
           (null owner)
           (subtypep (type-of owner) '<graph>)))
  (when (subtypep (type-of owner) '<graph>)
    (insert-to edge owner)))
