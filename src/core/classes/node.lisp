;;;; ./src/core/classes/node.lisp

(in-package :mnas-graph)

(defclass <node> (<node-attributes>)
  ((name
    :accessor name
    :initarg :name
    :initform nil
    :documentation "Имя вершины")
   (owner
    :accessor owner
    :initarg :owner
    :initform nil
    :documentation "Владелец вершины объект типа graph")
   (ht-inlet-edges
    :accessor ht-inlet-edges
    :initform (make-hash-table)
    :documentation "Хеш-таблица входящих ребер.")
   (ht-outlet-edges
    :accessor ht-outlet-edges
    :initform (make-hash-table)
    :documentation "Хеш-таблица исходящих ребер.")
;;;;   
   (counter
    :accessor <node>-counter
    :initform 0
    :documentation "Количество, созданных вершин"
    :allocation :class))
  (:documentation "@b(Описание:) класс @b(<node>) представляет вершину графа."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod initialize-instance :after ((x <node>) &key owner)
  (when owner (insert-to x owner))
  (incf (<node>-counter x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-object ((node <node>) s)
  (format s "~S [ " (name node)))

(defmethod print-object :after ((node <node>) s)
  (format s "]"))

