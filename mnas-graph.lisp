;;;; mnas-graph.lisp

(in-package #:cl-user)

(defpackage #:mnas-graph
  (:use #:cl #:mnas-hash-table))

(in-package #:mnas-graph)

;;;; generics ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export 'to-string)
(defgeneric to-string   (obj)           (:documentation "Выполняет перобразование объекта в строку"))

(export 'insert-to)
(defgeneric insert-to   (obj container) (:documentation "Добавляет obj в container"))

(export 'remove-from)
(defgeneric remove-from (obj container) (:documentation "Добавляет obj в container"))

(export 'inlet-vers)
(defgeneric inlet-vers (container) (:documentation "Возвращает хеш-таблицу конечных вершин (вершин-стока)"))

(export 'outlet-vers)
(defgeneric outlet-vers (container) (:documentation "Возвращает хеш-таблицу начальных вершин (веншин-иточников)"))

(export 'inlet-ribs)
(defgeneric inlet-ribs (container ver) (:documentation "Возвращает хеш-таблицу начальных ребер (итоков)"))

(export 'outlet-ribs)
(defgeneric outlet-ribs (container ver) (:documentation "Возвращает хеш-таблицу конечных ребер (устий)"))

(export 'to-graphviz)
(defgeneric to-graphviz (obj stream) (:documentation "Выполняет вывод в формат программы graphviz"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export 'ver)

(defclass ver ()
  ((node    :accessor vertex-node    :initarg :node   :initform nil  :documentation "Имя вершины")
   (counter :accessor vertex-counter                  :initform 0    :documentation "Количество, созданных вершин" :allocation :class))
   (:documentation "Вершина графа"))

(export 'vertex-node)
(export 'vertex-counter)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export 'vertex)

(defclass vertex (ver)
  ((number  :accessor vertex-number  :initarg :number                :documentation "Номер вершины")
   (state   :accessor vertex-state   :initarg :state  :initform nil  :documentation "Ссылка на состояние вершины"))
   (:documentation "Вершина графа поддерживающая номер и состояние"))

(export 'vertex-number)
(export 'vertex-state)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export 'rib)

(defclass rib ()
  ((start :accessor rib-from :initarg :from :initform nil :documentation "Начальная вершина ребра")
   (end   :accessor rib-to   :initarg :to   :initform nil :documentation "Конечная  вершина ребра"))
  (:documentation "Ребро графа"))


(export 'rib-from)
(export 'rib-to)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export 'graph)

(defclass graph ()
  ((vertexes :accessor graph-vers :initform (make-hash-table) :documentation "Хешированная таблица вершин графа")
   (ribs     :accessor graph-ribs :initform (make-hash-table) :documentation "Хешированная таблица ребер графа"))
  (:documentation "Представляет граф, выражающий алгоритм изменения состояния агрегатов во времени"))

(export 'graph-vers)
(export 'graph-ribs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod initialize-instance :around ((x ver) &key node )
  (call-next-method x
		    :node   node 
		    :number (vertex-counter x))
  (incf (vertex-counter x)))


(defmethod initialize-instance :around ((x vertex) &key vertex-node vertex-state)
  (call-next-method x
		    :vertex-node   vertex-node 
		    :vertex-number (vertex-counter x)
		    :vertex-state  vertex-state)
  (incf (vertex-counter x)))

;;;;;;;;;; print-object ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-object :after ((x ver) s)
	   (format s "~S(~S)~%" (vertex-counter x) (vertex-node x)))

(defmethod print-object :after ((x vertex) s)
	   (format s "(~S ~S)~%"
		   (vertex-number x)
		   (vertex-state  x)))

(defmethod print-object :after ((x rib) s)
  (format s "(~S->~S)" (rib-from x) (rib-to x)))

(defmethod print-object :after ((x graph) s)
  (format s "(VC=~S RC=~S"
	  (hash-table-count (graph-vers x))
	  (hash-table-count (graph-ribs x)))
  (when (< 0 (hash-table-count (graph-vers x)))
    (format s ")~%(" )
    (maphash #'(lambda (k v) (format s "~S " v) )(graph-vers x))
    (format s ")" ))
  (when (< 0 (hash-table-count (graph-ribs x)))
    	(format s "~%(" )
	(maphash #'(lambda (k v) (format s "~S~%" v) )(graph-ribs x))
	(format s ")"))
  (format s ")"))

;;;;;;;;;; to-string ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod to-string (val) (format nil "~A" val))

(defmethod to-string ((x ver)) (format nil "~A" (vertex-node x)))

(defmethod to-string ((x vertex)) (format nil "~A:~A" (vertex-node x) (vertex-number x)))

(defmethod to-string ((x rib))
  (format nil "~A->~A" (to-string (rib-from x)) (to-string (rib-to x))))

;;;;;;;;;; insert-to ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod insert-to ((v ver) (g graph)) (setf (gethash v (graph-vers g)) v) v)

(defmethod insert-to ((r rib) (g graph))
  (setf (gethash r (graph-ribs g)) r)
  (setf (gethash (rib-from r) (graph-vers g)) (rib-from r))
  (setf (gethash (rib-to   r) (graph-vers g)) (rib-to   r))
  r)

;;;;;;;;;; remove-from ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod remove-from ((v ver) (g graph ))
  (let* ((rh (graph-ribs g))
	 (rl (hash-table-copy rh)))
    (maphash #'(lambda(key val)
		 (if (or
		      (eq (rib-from key) v)
		      (eq (rib-to key)   v))
		     (remhash key rh)))
	     rl)
    (if (remhash v (graph-vers g))
	v)))

(defmethod remove-from ((r rib) (g graph ) )
  (if (remhash r (graph-ribs g))
	r))

;;;;;;;;;; graph-clear ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod graph-clear ((g graph))
  (clrhash (graph-vers g))
  (clrhash (graph-ribs g))
  g)


;;;;;;;;;;  graph-inlet graph-outlet ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod inlet-vers ((g graph))
  (let ((rez-tbl(hash-table-copy (graph-vers g))))
    (maphash
     #'(lambda (k v)
	 (remhash (rib-to k) rez-tbl))
     (graph-ribs g))
    rez-tbl))

(defmethod outlet-vers ((g graph))
  (let ((rez-tbl(hash-table-copy (graph-vers g))))
    (maphash
     #'(lambda (k v)
	 (remhash (rib-to k) rez-tbl))
     (graph-ribs g))
    rez-tbl))

(defmethod outlet-ribs ((g graph) (v ver))
  (let ((rez-tbl(hash-table-copy(graph-ribs g))))
    (maphash
     #'(lambda (key val)
	 (if (not(eq (rib-from key) v))
	     (remhash  key rez-tbl)))
     (graph-ribs g))
    rez-tbl))

(defmethod inlet-ribs ((g graph) (v ver))
  (let ((rez-tbl (hash-table-copy (graph-ribs g))))
    (maphash
     #'(lambda (key val)
	 (if (not(eq (rib-to key) v))
	     (remhash  key rez-tbl)))
     (graph-ribs g))
    rez-tbl))

;;;;;;;;;; graph-find-* ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export 'graph-find-vertex-by-name)

(defmethod graph-find-vertex-by-name ((g graph) (str string))
  (let ((ver nil))
    (maphash #'(lambda (k v)
	       (if (string= (to-string k) str)
		   (setf ver k))
	       )
	     (graph-vers g))
    ver))

(export 'graph-find-rib-by-name)

(defmethod graph-find-rib-by-name ((g graph) (str string))
  (let ((rb nil))
    (maphash #'(lambda (k v)
	       (if (string= (to-string k) str)
		   (setf rb k))
	       )
	     (graph-ribs g))
    rb))

;;;; to-graphviz ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod to-graphviz ((v ver) s)
  (format s "~S~%" (to-string v)))

(defmethod to-graphviz ((r rib) s)
  (format s "~S ~A ~S~%"
	  (to-string (rib-from r))
	  "->"
	  (to-string (rib-to r))))

(defun x-preamble(&key (out t) (name "G") (rankdir "LR") (shape "box"))
  (format out "digraph ~A {~%  rankdir=~A~%  node[shape=~A]~%" name rankdir shape))

(defun x-postamble(&key (out t)) (format out "~&}~%"))

(defmethod to-graphviz ((g graph) s)
  (x-preamble :out s)
  (maphash #'(lambda (k v) (to-graphviz v s)) (graph-vers g))  
  (maphash #'(lambda (k v) (to-graphviz v s)) (graph-ribs g))  
  (x-postamble :out s))

;;;; generate-graph data ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun generate-graph (data)
  (let ((g (make-instance 'graph))
	(vs (remove-duplicates (apply #'append data) :test #'equal)))
    (mapc #'(lambda (v) (insert-to (make-instance 'ver :node v) g)) vs)
    (mapc #'(lambda (el)
	      (insert-to
	       (make-instance 'rib
			      :from (graph-find-vertex-by-name g (first el))
			      :to   (graph-find-vertex-by-name g (second el)))
	       g))
	  data)
    g))
