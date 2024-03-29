;;;; ./src/core/classes/edge.lisp

(in-package :mnas-graph)

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
    "Конечная  вершина ребра")
   (weight
    :accessor weight
    :initarg :weight
    :initform 1
    :documentation
    "Определяет вес (длину) ребра."))
  (:documentation "@b(Описание:) класс @b(<edge>) представляет ребро графа.
                                                                                "))

(defmethod print-object ((edge <edge>) s)
    (format s "~S->~S ["
          (name (tail edge))
          (name (head edge))))

(defmethod print-object :after ((edge <edge>) s)
  (format s "]"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod initialize-instance :after ((edge <edge>) &key (owner))
  (assert (or
           (null owner)
           (subtypep (type-of owner) '<graph>)))
  (when (subtypep (type-of owner) '<graph>)
    (insert-to edge owner)))
