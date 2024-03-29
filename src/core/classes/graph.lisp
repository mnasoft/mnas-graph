(in-package :mnas-graph)

(defclass <graph> ()
  ((nodes
    :accessor nodes   :initform (make-hash-table)
    :documentation
    "Хешированная таблица вершин графа. Ключ - вершина; значение nil.")
   (ht-node-names
    :accessor ht-node-names :initform (make-hash-table :test #'equal)
    :documentation
    "Хешированная таблица имен вершин графа.")
   (edges
    :accessor edges   :initform (make-hash-table)
    :documentation
    "Хешированная таблица ребер графа. Ключ - ребро; значение - ребро.")
   (ht-edge-names
    :accessor ht-edge-names :initform (make-hash-table :test #'equal)
    :documentation
    "Хешированная таблица имен ребер графа. Ключ - строка; значение -
     ребро.")
;;;;   
   (rankdir
    :accessor rankdir :initform "TB"
    :documentation
    "Определяет направление отрисовки: TB, LR, BT, RL."))
  (:documentation "@b(Описание:) класс @b(<graph>) представляет граф.

                                                                                "))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-object        ((x <graph>) s))


(defmethod print-object :after ((x <graph>) s)
  (format s "#GRAPH(VC=~S RC=~S"
	  (hash-table-count (nodes x))
	  (hash-table-count (edges x)))
  (when (< 0 (hash-table-count (nodes x)))
    (format s ")~%(" )
    (maphash
     #'(lambda (key val)
	 val
	 (format s "~S " key))
     (nodes x))
    (format s ")" ))
  (when (< 0 (hash-table-count (edges x)))
    (format s "~%(" )
    (maphash
     #'(lambda (key val)
	 val
	 (format s "~S " key))
     (edges x))
    (format s ")"))
  (format s ")"))
