;;;; mnas-graph.lisp

(in-package #:cl-user)

(defpackage #:mnas-graph
  (:use #:cl #:mnas-hash-table))

(in-package #:mnas-graph)

(export 'vertex)

(defclass vertex ()
  ((vertex-node    :accessor vertex-node    :initarg :vertex-node   :initform nil :allocation :instance :documentation "Имя вершины")
   (vertex-number  :accessor vertex-number  :initarg :vertex-number               :allocation :instance :documentation "Номер вершины")
   (vertex-state   :accessor vertex-state   :initarg :vertex-state  :initform nil :allocation :instance :documentation "Ссылка на состояние вершины")
   (vertex-counter :accessor vertex-counter                         :initform 0   :allocation :class    :documentation "Количество, созданных вершин")))

(export 'vertex-node)
(export 'vertex-number)
(export 'vertex-state)
(export 'vertex-counter)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export 'rib)

(defclass rib ()
  ((rib-start-vertex :accessor rib-start-vertex :initarg :rib-start-vertex :initform nil :allocation :instance :documentation "Начальная вершина ребра")
   (rib-end-vertex   :accessor rib-end-vertex   :initarg :rib-end-vertex   :initform nil :allocation :instance :documentation "Конечная  вершина ребра"))
  (:documentation "Ребро графа"))


(export 'rib-start-vertex)
(export 'rib-end-vertex)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export 'graph)

(defclass graph ()
  ((graph-vertexes :accessor graph-vertexes :initform (make-hash-table) :documentation "Хешированная таблица вершин графа")
   (graph-ribs     :accessor graph-ribs     :initform (make-hash-table) :documentation "Хешированная таблица ребер графа"))
  (:documentation "Представляет граф, выражающий алгоритм изменения состояния агрегатов во времени"))

(export 'graph-vertexes)
(export 'graph-ribs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod initialize-instance :around ((x vertex) &key vertex-name vertex-node vertex-state)
  (call-next-method x
		    :vertex-name   vertex-name
		    :vertex-node   vertex-node 
		    :vertex-number (vertex-counter x)
		    :vertex-state  vertex-state)
  (incf (vertex-counter x)))

;;;;;;;;;; print-object ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-object :after ((x vertex) s)
	   (format s "(~S ~S ~S)~%"
		   (vertex-node   x)
		   (vertex-number x)
		   (vertex-state  x)))

(defmethod print-object :after ((x rib) s)
  (format s "((~S ~S ~S)->(~S ~S ~S))"
	  (vertex-node (rib-start-vertex x))
	  (vertex-number(rib-start-vertex x))
  	  (vertex-state (rib-start-vertex x))
	  (vertex-node(rib-end-vertex x))
	  (vertex-number(rib-end-vertex x))
  	  (vertex-state (rib-end-vertex x))))

(defmethod print-object :after ((x graph) s)
  (format s "(VC=~S RC=~S" (hash-table-count (graph-vertexes x)) (hash-table-count (graph-ribs x)))
  (if (< 0 (hash-table-count (graph-vertexes x)))
      (progn
        (format s ")~%(" )
	(maphash #'(lambda (k v) (format s "~S " v) )(graph-vertexes x))
        (format s ")" )))
  (if (< 0 (hash-table-count (graph-ribs x)))
      (progn 
	(format s "~%(" )
	(maphash #'(lambda (k v) (format s "~S~%" v) )(graph-ribs x))
	(format s ")")))
  (format s ")"))

;;;;;;;;;; to-string ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export 'to-string)

(defmethod to-string (val) (format nil "~A" val))

(defmethod to-string ((x vertex)) (format nil "~A:~A" (vertex-node x) (vertex-number x)))

(defmethod to-string ((x rib))
  (format nil "~A->~A" (to-string(rib-start-vertex x)) (to-string(rib-end-vertex x))))

;;;;;;;;;; graph-add-* ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export 'graph-add-vertex)

(defmethod graph-add-vertex ((g graph ) (v vertex)) (setf (gethash v (graph-vertexes g)) v) v)

(export 'graph-add-rib)

(defmethod graph-add-rib ((g graph ) (r rib))
  (setf (gethash r (graph-ribs g)) r)
  (setf (gethash (rib-start-vertex r) (graph-vertexes g)) (rib-start-vertex r))
  (setf (gethash (rib-end-vertex r) (graph-vertexes g)) (rib-end-vertex r))
  r)

;;;;;;;;;; graph-remove-* ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export 'graph-remove-vertex)

(defmethod graph-remove-vertex ((g graph ) (v vertex))
  (let* ((rh (graph-ribs g))
	 (rl (hash-table-copy rh)))
    (maphash #'(lambda(key val)
		 (if (or
		      (eq (rib-start-vertex key) v)
		      (eq (rib-end-vertex key)   v))
		     (remhash key rh)))
	     rl)
    (if (remhash v (graph-vertexes g))
	v)))

(export 'graph-remove-vertex)

(defmethod graph-remove-vertex ((g graph ) (r rib))
  (if (remhash r (graph-ribs g))
	r))



;;;;;;;;;; graph-clear ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod graph-clear ((g graph))
  (clrhash (graph-vertexes g))
  (clrhash (graph-ribs g))
  g)

;;;;;;;;;; graph-find-* ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export 'graph-find-inlet-vertexes)

(defmethod graph-find-inlet-vertexes ((g graph))
  (let ((rez-tbl(hash-table-copy(graph-vertexes g))))
    (maphash
     #'(lambda (k v)
	 (remhash (rib-end-vertex k) rez-tbl))
     (graph-ribs g))
    rez-tbl
    ))

(export 'graph-find-outlet-vertexes)

(defmethod graph-find-outlet-vertexes ((g graph))
  (let ((rez-tbl(hash-table-copy(graph-vertexes g))))
    (maphash
     #'(lambda (k v)
	 (remhash (rib-start-vertex k) rez-tbl))
     (graph-ribs g))
    rez-tbl
    ))

(defmethod graph-find-vertex-by-name((g graph) str)
  (let ((ver nil))
    (maphash #'(lambda (k v)
	       (if (string= (to-string k) str)
		   (setf ver k))
	       )
	     (graph-vertexes g))
    ver))

(export 'graph-find-rib-by-name)

(defmethod graph-find-rib-by-name ((g graph) str)
  (let ((rb nil))
    (maphash #'(lambda (k v)
	       (if (string= (to-string k) str)
		   (setf rb k))
	       )
	     (graph-ribs g))
    rb))

(export 'graph-find-outlet-ribs)

(defmethod graph-find-outlet-ribs ((g graph) (v vertex))
  (let ((rez-tbl(hash-table-copy(graph-ribs g))))
    (maphash
     #'(lambda (key val)
	 (if (not(eq (rib-start-vertex key) v))
	     (remhash  key rez-tbl)))
     (graph-ribs g))
    rez-tbl))
