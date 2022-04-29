;;;; mnas-graph.lisp

(defpackage #:mnas-graph
  (:use #:cl #:mnas-hash-table)
  (:export <node> 
           <edge>
           <graph>)
  (:export <node>-name
           <node>-owner
           <node>-counter)
  (:export <edge>-from
           <edge>-to)
  (:export <graph>-nodes
           <graph>-edges)
  ;; <node> 
  (:export connected-nodes
           nea-from-nodes
           nea-to-nodes
           inlet-edges
           outlet-edges
           )
  ;; <node> & <edge>
  (:export to-string)
  ;; <graph>
  (:export graph-clear
           inlet-nodes
           outlet-nodes
           find-node
           remove-from
           find-edge
           insert-to
           <graph>-nodes-count
           <graph>-edges-count
           )
  
  (:export make-graph
           make-random-graph
           )
  (:documentation
   " Пакет @b(mnas-graph) определяет базовые функции для создания
 структуры данных типа
 @link[uri='https://ru.wikipedia.org/wiki/Граф_(математика)'](Граф)
 и ее отображения через graphviz.

 Пакет определяет следующие основные классы: 
@begin(list)
@item(@ref[id=class-node](<node>) - вершина графа;)
@item(@ref[id=class-edge](<edge>) - ребро графа;)
@item(@ref[id=class-graph](<graph>) - граф.)  
@end(list)

 @b(Пример использования:)
@begin[lang=lisp](code)
  (let*
      ((g (make-instance 'mnas-graph:<graph>))
       (v1 (make-instance 'mnas-graph:<node> :owner g :name \"v1\"))
       (v2 (make-instance 'mnas-graph:<node> :owner g :name \"v2\"))
       (v3 (make-instance 'mnas-graph:<node> :owner g :name \"v3\"))
       (r1 (make-instance 'mnas-graph:<edge> :from v1 :to v2))
       (r2 (make-instance 'mnas-graph:<edge> :from v2 :to v3))
       (r3 (make-instance 'mnas-graph:<edge> :from v3 :to v1)))
    (mnas-graph:insert-to v1 g)
    (mnas-graph:insert-to v2 g)
    (mnas-graph:insert-to v3 g)
    (mnas-graph:insert-to r1 g)
    (mnas-graph:insert-to r2 g)
    (mnas-graph:insert-to r3 g)
    (mnas-graph:view-graph g))
@end(code)"))

;;;; (declaim (optimize (compilation-speed 0) (debug 3) (safety 0) (space 0) (speed 0)))
;;;; (declaim (optimize (space 0) (compilation-speed 0)  (speed 0) (safety 3) (debug 3)))
;;;; (setf sb-impl::*default-external-format* :utf8)

(in-package #:mnas-graph)

;;;; generics ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric to-string    (obj)
  (:documentation "Выполняет перобразование объекта в строку"))

(defgeneric insert-to    (obj container)
  (:documentation "Добавляет obj в container"))

(defgeneric remove-from  (obj container) (:documentation "Добавляет obj в container"))

(defgeneric inlet-nodes  (graph) (:documentation "Возвращает хеш-таблицу конечных вершин (вершин-стока)"))

(defgeneric outlet-nodes (graph) (:documentation "Возвращает хеш-таблицу начальных вершин (веншин-иточников)"))

(defgeneric inlet-edges  (node) (:documentation "Возвращает хеш-таблицу начальных ребер (истоков)"))

(defgeneric outlet-edges (node) (:documentation " Возвращает хеш-таблицу конечных ребер (устий)"))

(defgeneric find-node    (graph <node>-name) (:documentation "Поиск вершины по имени"))

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


(defgeneric nea-from-nodes  (node)         (:documentation "Возвращает хеш-таблицу вершин, с которыми соединена вершина node, в направлении от нее к ним"))

(defgeneric nea-to-nodes    (node)         (:documentation "Возвращает хеш-таблицу вершин, с которыми соединена вершина node, в направлении от них к ней"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass <node> ()
  ((name    :accessor <node>-name   :initarg :name  :initform nil :documentation "Имя вершины")
   (owner   :accessor <node>-owner  :initarg :owner :initform nil :documentation "Владелец вершины объект типа graph")
   (counter :accessor <node>-counter                :initform 0   :documentation "Количество, созданных вершин" :allocation :class))
  (:documentation "@b(Описание:) класс @b(<node>) представляет вершину графа.
                                                                                "))

(defclass <edge> ()
  ((start :accessor <edge>-from :initarg :from :initform nil :documentation "Начальная вершина ребра")
   (end   :accessor <edge>-to   :initarg :to   :initform nil :documentation "Конечная  вершина ребра"))
  (:documentation "@b(Описание:) класс @b(<edge>) представляет ребро графа.
                                                                                "))

(defclass <graph> ()
  ((nodes :accessor <graph>-nodes :initform (make-hash-table) :documentation "Хешированная таблица вершин графа")
   (edges :accessor <graph>-edges :initform (make-hash-table) :documentation "Хешированная таблица ребер графа"))
  (:documentation "@b(Описание:) класс @b(<graph>) представляет граф.
                                                                                "))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod initialize-instance :around ((x <node>) &key name (owner nil))
  (call-next-method x
		    :name   name
    		    :owner  owner 
		    :number (<node>-counter x))
  (when owner (insert-to x owner))
  (incf (<node>-counter x)))

;;;;;;;;;; print-object ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-object        ((x <node>) s))

(defmethod print-object :after ((x <node>) s)
  (format s "~S:~S"   (not(null (<node>-owner x))) (<node>-name x)))

(defmethod print-object        ((x <edge>) s))

(defmethod print-object :after ((x <edge>) s)
  (format s "(~S->~S)" (<edge>-from x) (<edge>-to x)))

(defmethod print-object        ((x <graph>) s))

(defmethod print-object :after ((x <graph>) s)
  (format s "#GRAPH(VC=~S RC=~S"
	  (hash-table-count (<graph>-nodes x))
	  (hash-table-count (<graph>-edges x)))
  (when (< 0 (hash-table-count (<graph>-nodes x)))
    (format s ")~%(" )
    (maphash
     #'(lambda (key val)
	 val
	 (format s "~S " key))
     (<graph>-nodes x))
    (format s ")" ))
  (when (< 0 (hash-table-count (<graph>-edges x)))
    (format s "~%(" )
    (maphash
     #'(lambda (key val)
	 val
	 (format s "~S " key))
     (<graph>-edges x))
    (format s ")"))
  (format s ")"))

;;;;;;;;;; to-string ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod to-string (val)
  "@b(Описание:) to-string !!!!!!
"
  (format nil "~A" val))

(defmethod to-string ((x <node>))
  "@b(Описание:) to-string !!!!!!
"
  (format nil "~A" (<node>-name x)))

(defmethod to-string ((x <edge>))
  "@b(Описание:) to-string !!!!!!
"
  (format nil "~A->~A" (to-string (<edge>-from x)) (to-string (<edge>-to x))))

;;;;;;;;;; insert-to ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod insert-to ((n <node>) (g <graph>))
  "@b(Описание:) insert-to !!!!!!
"
  (setf (gethash n (<graph>-nodes g)) n
	(<node>-owner n) g)
  n)

(defmethod insert-to ((e <edge>) (g <graph>))
  "@b(Описание:) insert-to ((e <edge>) (g <graph>))!!!!!!
"
  (setf (gethash e (<graph>-edges g)) e)
  (setf (<node>-owner (<edge>-from e)) g)
  (setf (<node>-owner (<edge>-to   e)) g)
  (setf (gethash (<edge>-from e) (<graph>-nodes g)) (<edge>-from e))
  (setf (gethash (<edge>-to   e) (<graph>-nodes g)) (<edge>-to   e))
  e)

;;;;;;;;;; remove-from ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod remove-from ((n <node>) (g <graph> ))
  "@b(Описание:) remove-from ((n <node>) (g <graph> ))!!!!!!
"
  (let* ((rh (<graph>-edges g))
	 (rl (hash-table-copy rh)))
    (maphash #'(lambda(key val)
		 val
		 (if (or
		      (eq (<edge>-from key) n)
		      (eq (<edge>-to key)   n))
		     (remhash key rh)))
	     rl)
    (if (remhash n (<graph>-nodes g))
	n)))

(defmethod remove-from ((e <edge>) (g <graph> ) )
  "@b(Описание:) remove-from ((e <edge>) (g <graph> ) )!!!!!!
"
  (if (remhash e (<graph>-edges g))
      e))

;;;;;;;;;; graph-clear ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod graph-clear ((g <graph>))
  "@b(Описание:) graph-clear ((g <graph>))!!!!!!
"
  (clrhash (<graph>-nodes g))
  (clrhash (<graph>-edges g))
  g)

;;;;;;;;;;  inlet outlet ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod outlet-edges ((n <node>) &aux (g (<node>-owner n)))
  "@b(Описание:) outlet-edges ((n <node>) &aux (g (<node>-owner n)))!!!!!!
"
  (let ((rez-tbl(hash-table-copy (<graph>-edges g))))
    (maphash
     #'(lambda (key val)
	 val
	 (if (not(eq (<edge>-from key) n))
	     (remhash  key rez-tbl)))
     (<graph>-edges g))
    rez-tbl))

(export 'inlet-edges )

(defmethod inlet-edges ((n <node>) &aux (g (<node>-owner n)))
  "@b(Описание:) inlet-edges ((n <node>) &aux (g (<node>-owner n)))!!!!!!
"
  (let ((rez-tbl (hash-table-copy (<graph>-edges g))))
    (maphash
     #'(lambda (key val)
	 val
	 (if (not(eq (<edge>-to key) n))
	     (remhash  key rez-tbl)))
     (<graph>-edges g))
    rez-tbl))

;;;;;;;;;; find-* ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod find-node ((g <graph>) (str string))
  "@b(Описание:) find-node ((g <graph>) (str string))!!!!!!
"
  (let ((v-rez nil))
    (maphash #'(lambda (key val)
		 val
	         (if (string= (to-string key) str)
		     (setf v-rez key)))
	     (<graph>-nodes g))
    v-rez))

(defmethod find-edge ((g <graph>) (str string))
  "@b(Описание:) find-edge ((g <graph>) (str string))!!!!!!
"
  (let ((e-rez nil))
    (maphash #'(lambda (key val)
		 val
	         (if (string= (to-string key) str)
		     (setf e-rez key)))
	     (<graph>-edges g))
    e-rez))

;;;; make-graph data ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-graph (edges &key nodes)
  "@b(Описание:) make-graph возвращает граф с ребрами edges и вершинами вершинами nodes."
  (let ((g (make-instance '<graph>))
	(vs (remove-duplicates (append (apply #'append edges) nodes) :test #'equal)))
    (mapc #'(lambda (v) (insert-to (make-instance '<node> :name v) g)) vs)
    (mapc #'(lambda (el)
	      (insert-to
	       (make-instance '<edge>
			      :from (find-node g (first el))
			      :to   (find-node g (second el)))
	       g))
	  edges)
    g))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-random-graph (&key (node-max-number 100) (edges-number node-max-number))
  "@b(Описание:) make-random-graph создает случайный граф.

 @b(Переменые:)
@begin(list)
 @item(node-max-number - количество вершин графа;)
 @item(edges-number    - количество вершин графа.)
@end(list)
@b(Пример использования:)
@begin[lang=lisp](code)
 (make-random-graph)
@end(code)
"
  (make-graph
   (let ((lst nil))
     (dotimes (i edges-number lst)
       (push (list
	      (format nil "~A" (random node-max-number))
	      (format nil "~A" (random node-max-number)))
	     lst)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod nea-to-nodes  ((n <node>) &aux (ht (make-hash-table)))
  "@b(Описание:) nea-to-nodes возвращает хеш-таблицу вершин, с которыми соединена вершина <node>, в направлении от нее к ним.
"
  (maphash
   #'(lambda (key val)
       val
       (setf (gethash (<edge>-to key) ht) (<edge>-to key)))
   (outlet-edges n))
  (print-items ht)
  ht)

(defmethod nea-from-nodes  ((n <node>) &aux (ht (make-hash-table)))
  "@b(Описание:) nea-from-nodes
Возвращает хеш-таблицу вершин, с которыми соединена вершина <node>, в направлении от них к ней.
"
  (maphash
   #'(lambda (key val)
       val
       (setf (gethash (<edge>-from key) ht) (<edge>-from key)))
   (inlet-edges n))
  (print-items ht)
  ht)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod connected-nodes ((n <node>) &key (direction :direction-to) &aux (ht (make-hash-table )))
  "@b(Описание:) connected-nodes ((n <node>) &key (direction :direction-to) &aux (ht (make-hash-table )))!!!!!!
"
  (setf (gethash n ht) n)
  (do ((count-before -1) (count-after  0))
      ((= count-before count-after) ht)
    (setf count-before (hash-table-count ht))
    (when (eq direction :direction-to)
      (maphash #'(lambda (key val)
		   val
		   (maphash
		    #'(lambda (key val)
			val
			(setf (gethash  key ht) key))
		    (nea-to-nodes key)))
	       ht))
    (when (eq direction :direction-from)
      (maphash #'(lambda (key val)
		   val
		   (maphash
		    #'(lambda (key val)
			val
			(setf (gethash  key ht) key))
		    (nea-from-nodes key)))
	       ht))
    (setf count-after (hash-table-count ht))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod <graph>-nodes-count ((graph <graph>))
  " @b(Описание:) метод @b(<graph>-nodes-count) возвращает количество вершин графа @b(graph)."
  (hash-table-count (<graph>-nodes graph)))

(defmethod <graph>-edges-count ((graph <graph>))
  " @b(Описание:) метод @b(<graph>-nodes-count) возвращает количество ребер графа @b(graph)."
  (hash-table-count (<graph>-edges graph)))
