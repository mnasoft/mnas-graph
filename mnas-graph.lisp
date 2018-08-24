;;;; mnas-graph.lisp

(in-package #:cl-user)

(defpackage #:mnas-graph
  (:use #:cl #:mnas-hash-table)
  (:intern graphviz-prg)
  (:export to-string
	   insert-to    remove-from
	   inlet-nodes  outlet-nodes
	   inlet-ribs   outlet-ribs
	   find-node    find-edge
	   to-graphviz
	   view-graph
	   node         node-name node-counter
	   edge         edge-from edge-to
	   graph        graph-nodes  graph-edges
	   )
  (:export *filter-dot* *filter-neato* *filter-twopi* *filter-circo* *filter-fdp* *filter-sfdp* *filter-patchwork*)
  (:export
  *output-path*
  *viewer-path*
  make-graph make-random-graph)
  (:export demo-1 demo-2 demo-3 demo-4 demo-5 ))

(declaim (optimize (space 0) (compilation-speed 0)  (speed 0) (safety 3) (debug 3)))

(in-package #:mnas-graph)

;;;; generics ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric to-string    (obj)           (:documentation "Выполняет перобразование объекта в строку"))

(defgeneric insert-to    (obj container) (:documentation "Добавляет obj в container"))

(defgeneric remove-from  (obj container) (:documentation "Добавляет obj в container"))

(defgeneric inlet-nodes  (container)     (:documentation "Возвращает хеш-таблицу конечных вершин (вершин-стока)"))

(defgeneric outlet-nodes (container)     (:documentation "Возвращает хеш-таблицу начальных вершин (веншин-иточников)"))

(defgeneric inlet-ribs   (graph node)    (:documentation "Возвращает хеш-таблицу начальных ребер (итоков)"))

(defgeneric outlet-ribs  (graph node)    (:documentation "Возвращает хеш-таблицу конечных ребер (устий)"))

(defgeneric outlet-edges (graph node)    (:documentation "non"))

(defgeneric inlet-edges  (graph node)    (:documentation "non"))

(defgeneric find-node    (graph node-name) (:documentation "Поиск вершины по имени"))

(defgeneric find-edge    (graph edge-name) (:documentation "Поиск ребра по имени"))

(defgeneric to-graphviz  (obj stream) (:documentation "Выполняет объекта obj в формат программы graphviz"))

(defgeneric view-graph (graph &key fpath fname graphviz-prg out-type dpi viewer)
  (:documentation "Выполняет визуализацию графа graph
fpath         - каталог для вывода результатов работы программы;
fname         - имя gv-файла;
out-type      - тип выходного файла;
dpi           - количество точек на дюйм;
viewer        - программа для просмотра графа;
graphviz-prg  - программа для генерации графа;
 :filter-dot
 :filter-neato
 :filter-twopi
 :filter-circo
 :filter-fdp  
 :filter-sfdp 
 :filter-patchwork 
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

(defmethod print-object        ((x node) s))
(defmethod print-object :after ((x node) s)
	   (format s "~S"  (node-name x)))

(defmethod print-object        ((x edge) s))
(defmethod print-object :after ((x edge) s)
  (format s "(~S->~S)" (edge-from x) (edge-to x)))

(defmethod print-object        ((x graph) s))
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

(defmethod print-object :after ((x graph) s)
  (format s "#GRAPH(VC=~S RC=~S"
	  (hash-table-count (graph-nodes x))
	  (hash-table-count (graph-edges x)))
  (when (< 0 (hash-table-count (graph-nodes x)))
    (format s ")~%(" )
    (maphash
     #'(lambda (key val)
	 val
	 (format s "~S " key))
     (graph-nodes x))
    (format s ")" ))
  (when (< 0 (hash-table-count (graph-edges x)))
    (format s "~%(" )
    (maphash
     #'(lambda (key val)
	 val
	 (format s "~S " key))
     (graph-edges x))
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
	 v
	 (remhash (edge-from k) rez-tbl))
     (graph-edges g))
    rez-tbl))

(defmethod outlet-nodes ((g graph))
  (let ((rez-tbl(hash-table-copy (graph-nodes g))))
    (maphash
     #'(lambda (k v)
	 v
	 (remhash (edge-to k) rez-tbl))
     (graph-edges g))
    rez-tbl))

(defmethod outlet-edges ((g graph) (v node))
  (let ((rez-tbl(hash-table-copy(graph-edges g))))
    (maphash
     #'(lambda (key val)
	 val
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

;;;;;;;;;; find-* ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod find-node ((g graph) (str string))
  (let ((v-rez nil))
    (maphash #'(lambda (k v)
	       (if (string= (to-string k) str)
		   (setf v-rez k))
	       )
	     (graph-nodes g))
    v-rez))

(defmethod find-edge ((g graph) (str string))
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

;;;; make-graph data ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-graph (edges &key nodes)
  (let ((g (make-instance 'graph))
	(vs (remove-duplicates (append (apply #'append edges) nodes) :test #'equal)))
    (mapc #'(lambda (v) (insert-to (make-instance 'node :name v) g)) vs)
    (mapc #'(lambda (el)
	      (insert-to
	       (make-instance 'edge
			      :from (find-node g (first el))
			      :to   (find-node g (second el)))
	       g))
	  edges)
    g))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *filter-dot* 
  (cond ((uiop/os:os-windows-p) "D:/PRG/msys32/mingw32/bin/dot.exe")
	((uiop/os:os-unix-p) "/usr/bin/dot"))
  "dot       - filter for drawing directed graphs")

(defparameter *filter-neato*
  (cond ((uiop/os:os-windows-p) "d:/PRG/msys32/mingw32/bin/neato.exe")
	((uiop/os:os-unix-p) "/usr/bin/neato"))
  "neato     - filter for drawing undirected graphs")

(defparameter *filter-twopi*
  (cond ((uiop/os:os-windows-p) "d:/PRG/msys32/mingw32/bin/twopi.exe")
	((uiop/os:os-unix-p) "/usr/bin/twopi"))
  "twopi     - filter for radial layouts of graphs")

(defparameter *filter-circo*
  (cond ((uiop/os:os-windows-p) "d:/PRG/msys32/mingw32/bin/circo.exe")
	((uiop/os:os-unix-p) "/usr/bin/circo"))
  "circo     - filter for circular layout of graphs")

(defparameter *filter-fdp*
  (cond ((uiop/os:os-windows-p) "d:/PRG/msys32/mingw32/bin/fdp.exe")
	((uiop/os:os-unix-p) "/usr/bin/fdp"))
  "fdp       - filter for drawing undirected graphs")

(defparameter *filter-sfdp*
  (cond ((uiop/os:os-windows-p) "d:/PRG/msys32/mingw32/bin/sfdp.exe")
	((uiop/os:os-unix-p) "/usr/bin/sfdp"))
  "sfdp      - filter for drawing large undirected graphs")

(defparameter *filter-patchwork*
  (cond ((uiop/os:os-windows-p) "d:/PRG/msys32/mingw32/bin/patchwork.exe")
	((uiop/os:os-unix-p) "/usr/bin/patchwork"))
  "patchwork - filter for tree maps")

(defun graphviz-prg (key)
  (when (symbolp key)
    (ecase key
      (:filter-dot       *filter-dot*      )
      (:filter-neato     *filter-neato*    )
      (:filter-twopi     *filter-twopi*    )
      (:filter-circo     *filter-circo*    )
      (:filter-fdp       *filter-fdp*      )
      (:filter-sfdp      *filter-sfdp*     )
      (:filter-patchwork *filter-patchwork*))))

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
			 (graphviz-prg :filter-dot)
			 (out-type "pdf")
			 (dpi "300")
			 (viewer *viewer-path*))
  (with-open-file (out (concatenate 'string fpath "/" fname ".gv")
		       :direction :output :if-exists :supersede :external-format :UTF8)
    (to-graphviz g out))
  (sb-ext:run-program (graphviz-prg graphviz-prg)
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
  (make-graph
   (let ((lst nil))
     (dotimes (i edges-number lst)
       (push (list
	      (format nil "~A" (random node-max-number))
	      (format nil "~A" (random node-max-number)))
	     lst)))))

