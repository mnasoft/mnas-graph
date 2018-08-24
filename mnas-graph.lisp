
;;;; mnas-graph.lisp

(in-package #:cl-user)

(defpackage #:mnas-graph
  (:use #:cl #:mnas-hash-table)
  (:export to-string
	   insert-to
	   remove-from
	   inlet-nodes
	   outlet-nodes
	   inlet-ribs
	   outlet-ribs
	   graph-find-node-by-name
	   graph-find-edge-by-name
	   to-graphviz
	   view-graph
	   node
	   node-name
	   node-counter
	   vertex
	   vertex-number
	   vertex-state
	   edge
	   edge-from
	   edge-to
	   graph
	   graph-nodes
	   graph-edges
	   generate-graph
	   *dot-path*
	   *neato-path*
	   *twopi-path*
	   *circo-path*
	   *fdp-path*
	   *sfdp-path*
	   *patchwork*
	   *output-path*
	   *viewer-path*
	   make-random-graph))

(declaim (optimize (space 0) (compilation-speed 0)  (speed 0) (safety 3) (debug 3)))

(in-package #:mnas-graph)

;;;; generics ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric to-string   (obj)           (:documentation "Выполняет перобразование объекта в строку"))

(defgeneric insert-to   (obj container) (:documentation "Добавляет obj в container"))

(defgeneric remove-from (obj container) (:documentation "Добавляет obj в container"))

(defgeneric inlet-nodes (container) (:documentation "Возвращает хеш-таблицу конечных вершин (вершин-стока)"))

(defgeneric outlet-nodes (container) (:documentation "Возвращает хеш-таблицу начальных вершин (веншин-иточников)"))

(defgeneric inlet-ribs (container node) (:documentation "Возвращает хеш-таблицу начальных ребер (итоков)"))

(defgeneric outlet-ribs (container node) (:documentation "Возвращает хеш-таблицу конечных ребер (устий)"))

(defgeneric graph-find-node-by-name (container node-name) (:documentation "Поиск вершины по имени"))

(defgeneric graph-find-edge-by-name (container edge-name) (:documentation "Поиск ребра по имени"))

(defgeneric to-graphviz (obj stream) (:documentation "Выполняет объекта obj в формат программы graphviz"))

(defgeneric view-graph (graph &key fpath fname graphviz-prg out-type dpi viewer)
  (:documentation "Выполняет визуализацию графа graph
fpath         - каталог для вывода результатов работы программы;
fname         - имя gv-файла;
out-type      - тип выходного файла;
dpi           - количество точек на дюйм;
viewer        - программа для просмотра графа;
graphviz-prg  - программа для генерации графа;
"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass node ()
  ((name    :accessor node-name    :initarg :name   :initform nil  :documentation "Имя вершины")
   (counter :accessor node-counter                  :initform 0    :documentation "Количество, созданных вершин" :allocation :class))
   (:documentation "Вершина графа"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass edge ()
  ((start :accessor edge-from :initarg :from :initform nil :documentation "Начальная вершина ребра")
   (end   :accessor edge-to   :initarg :to   :initform nil :documentation "Конечная  вершина ребра"))
  (:documentation "Ребро графа"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass graph ()
  ((vertexes  :accessor graph-nodes :initform (make-hash-table) :documentation "Хешированная таблица вершин графа")
   (edges     :accessor graph-edges :initform (make-hash-table) :documentation "Хешированная таблица ребер графа"))
  (:documentation "Представляет граф, выражающий алгоритм изменения состояния агрегатов во времени"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod initialize-instance :around ((x node) &key name )
  (call-next-method x
		    :name   name 
		    :number (node-counter x))
  (incf (node-counter x)))


;;;;;;;;;; print-object ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-object :after ((x node) s)
	   (format s "~S(~S)~%" (node-counter x) (node-name x)))

(defmethod print-object :after ((x edge) s)
  (format s "(~S->~S)" (edge-from x) (edge-to x)))

(defmethod print-object :after ((x graph) s)
  (format s "(VC=~S RC=~S"
	  (hash-table-count (graph-nodes x))
	  (hash-table-count (graph-edges x)))
  (when (< 0 (hash-table-count (graph-nodes x)))
    (format s ")~%(" )
    (maphash #'(lambda (k v) (format s "~S " v) )(graph-nodes x))
    (format s ")" ))
  (when (< 0 (hash-table-count (graph-edges x)))
    	(format s "~%(" )
	(maphash #'(lambda (k v) (format s "~S~%" v) )(graph-edges x))
	(format s ")"))
  (format s ")"))

;;;;;;;;;; to-string ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod to-string (val) (format nil "~A" val))

(defmethod to-string ((x node)) (format nil "~A" (node-name x)))

(defmethod to-string ((x edge))
  (format nil "~A->~A" (to-string (edge-from x)) (to-string (edge-to x))))

;;;;;;;;;; insert-to ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod insert-to ((v node) (g graph)) (setf (gethash v (graph-nodes g)) v) v)

(defmethod insert-to ((r edge) (g graph))
  (setf (gethash r (graph-edges g)) r)
  (setf (gethash (edge-from r) (graph-nodes g)) (edge-from r))
  (setf (gethash (edge-to   r) (graph-nodes g)) (edge-to   r))
  r)

;;;;;;;;;; remove-from ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod remove-from ((v node) (g graph ))
  (let* ((rh (graph-edges g))
	 (rl (hash-table-copy rh)))
    (maphash #'(lambda(key val)
		 (if (or
		      (eq (edge-from key) v)
		      (eq (edge-to key)   v))
		     (remhash key rh)))
	     rl)
    (if (remhash v (graph-nodes g))
	v)))

(defmethod remove-from ((r edge) (g graph ) )
  (if (remhash r (graph-edges g))
	r))

;;;;;;;;;; graph-clear ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod graph-clear ((g graph))
  (clrhash (graph-nodes g))
  (clrhash (graph-edges g))
  g)


;;;;;;;;;;  inlet outlet ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod inlet-nodes ((g graph))
  (let ((rez-tbl(hash-table-copy (graph-nodes g))))
    (maphash
     #'(lambda (k v)
	 (remhash (edge-to k) rez-tbl))
     (graph-edges g))
    rez-tbl))

(defmethod outlet-nodes ((g graph))
  (let ((rez-tbl(hash-table-copy (graph-nodes g))))
    (maphash
     #'(lambda (k v)
	 (remhash (edge-to k) rez-tbl))
     (graph-edges g))
    rez-tbl))

(defmethod outlet-edges ((g graph) (v node))
  (let ((rez-tbl(hash-table-copy(graph-edges g))))
    (maphash
     #'(lambda (key val)
	 (if (not(eq (edge-from key) v))
	     (remhash  key rez-tbl)))
     (graph-edges g))
    rez-tbl))

(defmethod inlet-edges ((g graph) (v node))
  (let ((rez-tbl (hash-table-copy (graph-edges g))))
    (maphash
     #'(lambda (key val)
	 (if (not(eq (edge-to key) v))
	     (remhash  key rez-tbl)))
     (graph-edges g))
    rez-tbl))

;;;;;;;;;; graph-find-* ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod graph-find-node-by-name ((g graph) (str string))
  (let ((v-rez nil))
    (maphash #'(lambda (k v)
	       (if (string= (to-string k) str)
		   (setf v-rez k))
	       )
	     (graph-nodes g))
    v-rez))

(defmethod graph-find-edge-by-name ((g graph) (str string))
  (let ((e-rez nil))
    (maphash #'(lambda (k v)
	       (if (string= (to-string k) str)
		   (setf e-rez k))
	       )
	     (graph-edges g))
    e-rez))

;;;; to-graphviz ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod to-graphviz ((v node) s)
  (format s "~S~%" (to-string v)))

(defmethod to-graphviz ((r edge) s)
  (format s "~S ~A ~S~%"
	  (to-string (edge-from r))
	  "->"
	  (to-string (edge-to r))))

(defun x-preamble(&key (out t) (name "G") (rankdir "LR") (shape "box"))
  (format out "digraph ~A {~%  rankdir=~A~%  node[shape=~A]~%" name rankdir shape))

(defun x-postamble(&key (out t)) (format out "~&}~%"))

(defmethod to-graphviz ((g graph) s)
  (x-preamble :out s)
  (maphash #'(lambda (k v) (to-graphviz v s)) (graph-nodes g))  
  (maphash #'(lambda (k v) (to-graphviz v s)) (graph-edges g))  
  (x-postamble :out s))

;;;; generate-graph data ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun generate-graph (data)
  (let ((g (make-instance 'graph))
	(vs (remove-duplicates (apply #'append data) :test #'equal)))
    (mapc #'(lambda (v) (insert-to (make-instance 'node :name v) g)) vs)
    (mapc #'(lambda (el)
	      (insert-to
	       (make-instance 'edge
			      :from (graph-find-node-by-name g (first el))
			      :to   (graph-find-node-by-name g (second el)))
	       g))
	  data)
    g))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *dot-path* 
  (cond ((uiop/os:os-windows-p) "D:/PRG/msys32/mingw32/bin/dot.exe")
	((uiop/os:os-unix-p) "/usr/bin/dot"))
  "dot       - filter for drawing directed graphs")

(defparameter *neato-path*
  (cond ((uiop/os:os-windows-p) "d:/PRG/msys32/mingw32/bin/neato.exe")
	((uiop/os:os-unix-p) "/usr/bin/neato"))
  "neato     - filter for drawing undirected graphs")

(defparameter *twopi-path*
  (cond ((uiop/os:os-windows-p) "d:/PRG/msys32/mingw32/bin/twopi.exe")
	((uiop/os:os-unix-p) "/usr/bin/twopi"))
  "twopi     - filter for radial layouts of graphs")

(defparameter *circo-path*
  (cond ((uiop/os:os-windows-p) "d:/PRG/msys32/mingw32/bin/circo.exe")
	((uiop/os:os-unix-p) "/usr/bin/circo"))
  "circo     - filter for circular layout of graphs")

(defparameter *fdp-path*
  (cond ((uiop/os:os-windows-p) "d:/PRG/msys32/mingw32/bin/fdp.exe")
	((uiop/os:os-unix-p) "/usr/bin/fdp"))
  "fdp       - filter for drawing undirected graphs")

(defparameter *sfdp-path*
  (cond ((uiop/os:os-windows-p) "d:/PRG/msys32/mingw32/bin/sfdp.exe")
	((uiop/os:os-unix-p) "/usr/bin/sfdp"))
  "sfdp      - filter for drawing large undirected graphs")

(defparameter *patchwork*
  (cond ((uiop/os:os-windows-p) "d:/PRG/msys32/mingw32/bin/patchwork.exe")
	((uiop/os:os-unix-p) "/usr/bin/patchwork"))
  "patchwork - filter for tree maps")

(defparameter *filter-for-drawing-lst* 
  '(:dot :neato :twopi :circo :fdp :sfdp :patchwork))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *output-path*
  (cond ((uiop/os:os-windows-p) "D:/PRG/msys32/home/namatv") 
	((uiop/os:os-unix-p) "/home/namatv")))

(defparameter *viewer-path*
  (cond
    ((uiop/os:os-windows-p)
     (cond
       ((probe-file "C:/Program Files (x86)/Adobe/Acrobat Reader DC/Reader/AcroRd32.exe") "C:/Program Files (x86)/Adobe/Acrobat Reader DC/Reader/AcroRd32.exe")
       ((probe-file "C:/Program Files/Adobe/Reader 11.0/Reader/AcroRd32.exe") "C:/Program Files/Adobe/Reader 11.0/Reader/AcroRd32.exe")))
    ((uiop/os:os-unix-p)
     (cond
       ((probe-file "/usr/bin/atril") "/usr/bin/atril")
       ((probe-file "/usr/bin/atril") "/usr/bin/okular")))))

(defmethod view-graph ((g graph) 
		       &key
			 (fpath *output-path*)
			 (fname "graph")
			 (graphviz-prg *dot-path*)
			 (out-type "pdf")
			 (dpi "300")
			 (viewer *viewer-path*))
  (with-open-file (out (concatenate 'string fpath "/" fname ".gv")
		       :direction :output :if-exists :supersede :external-format :UTF8)
    (to-graphviz g out))
  (sb-ext:run-program graphviz-prg
		      (list (concatenate 'string "-T" out-type)
			    (concatenate 'string "-Gdpi=" dpi)
			    "-o"
			    (concatenate 'string fpath "/" fname ".gv" "." out-type)
			    (concatenate 'string fpath "/" fname ".gv")))
  (when viewer
    (sb-ext:run-program viewer
			(list (concatenate 'string fpath "/" fname ".gv" "." out-type))))
  g)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-random-graph (&key (node-max-number 100) (edges-number node-max-number))
  "Описание"
  (generate-graph
   (let ((lst nil))
     (dotimes (i edges-number lst)
       (push (list
	      (format nil "~A" (random node-max-number))
	      (format nil "~A" (random node-max-number)))
	     lst)))))

