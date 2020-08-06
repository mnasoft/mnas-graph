;;;; mnas-graph.lisp

(in-package :cl-user)

(defpackage #:mnas-graph
  (:use #:cl #:mnas-hash-table))

;;;; (declaim (optimize (compilation-speed 0) (debug 3) (safety 0) (space 0) (speed 0)))

(setf sb-impl::*default-external-format* :utf8)

(in-package #:mnas-graph)

(declaim (optimize (space 0) (compilation-speed 0)  (speed 0) (safety 3) (debug 3)))

(annot:enable-annot-syntax)

(defparameter *graph-count* -1
  "Счетчик созданных вершин, ребер, графов")

@export
@export
(defparameter *output-path*
  (cond ((uiop/os:os-windows-p) (namestring (probe-file "~/."))) 
	((uiop/os:os-unix-p) (namestring (probe-file "~/."))))
  "Каталог для вывода файлов Графа по-умолчанию.")

@export
(defparameter *viewer-path*
  (cond
    ((uiop/os:os-windows-p)
     (cond
       ((probe-file "C:/Program Files (x86)/Adobe/Acrobat Reader DC/Reader/AcroRd32.exe") "C:/Program Files (x86)/Adobe/Acrobat Reader DC/Reader/AcroRd32.exe")
       ((probe-file "C:/Program Files/Adobe/Reader 11.0/Reader/AcroRd32.exe") "C:/Program Files/Adobe/Reader 11.0/Reader/AcroRd32.exe")))
    ((uiop/os:os-unix-p)
     (cond
       ((probe-file "/usr/bin/xdg-open") "/usr/bin/xdg-open")
       ((probe-file "/usr/bin/atril") "/usr/bin/atril")
       ((probe-file "/usr/bin/atril") "/usr/bin/okular")
       )))
  "Программа просмотрщик Графа")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-filter-prg-path (program)
  (cond
    ((and (uiop/os:os-windows-p)
	  (uiop:getenv "MSYSTEM_PREFIX"))
     (concatenate 'string (uiop:getenv "MSYSTEM_PREFIX") "/" "bin" "/" program "." "exe"))
    ((uiop/os:os-unix-p)
     (concatenate 'string "/usr/bin/" program))))

@export
@annot.doc:doc
"dot       - filter for drawing directed graphs"
(defparameter *filter-dot* (make-filter-prg-path "dot"))

@export
@annot.doc:doc
"neato     - filter for drawing undirected graphs"
(defparameter *filter-neato* (make-filter-prg-path "neato"))

@export
@annot.doc:doc
"twopi     - filter for radial layouts of graphs"
(defparameter *filter-twopi* (make-filter-prg-path "twopi"))

@export
@annot.doc:doc
"circo     - filter for circular layout of graphs"
(defparameter *filter-circo* (make-filter-prg-path "circo"))

@export
@annot.doc:doc
"fdp       - filter for drawing undirected graphs"
(defparameter *filter-fdp* (make-filter-prg-path "fdp"))

@export
@annot.doc:doc
"sfdp      - filter for drawing large undirected graphs"
(defparameter *filter-sfdp* (make-filter-prg-path "sfdp"))

@export
@annot.doc:doc
"patchwork - filter for tree maps"
(defparameter *filter-patchwork* (make-filter-prg-path "patchwork"))

;;;; generics ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@export
(defgeneric to-string    (obj)             (:documentation "Выполняет перобразование объекта в строку"))

@export
(defgeneric insert-to    (obj container)   (:documentation "Добавляет obj в container"))

@export
(defgeneric remove-from  (obj container)   (:documentation "Добавляет obj в container"))

@export
(defgeneric inlet-nodes  (graph)           (:documentation "Возвращает хеш-таблицу конечных вершин (вершин-стока)"))

@export
(defgeneric outlet-nodes (graph)           (:documentation "Возвращает хеш-таблицу начальных вершин (веншин-иточников)"))

@export
(defgeneric inlet-edges  (node)            (:documentation "Возвращает хеш-таблицу начальных ребер (итоков)"))

@export
(defgeneric outlet-edges (node)            (:documentation "Возвращает хеш-таблицу конечных ребер (устий)"))

@export
(defgeneric find-node    (graph node-name) (:documentation "Поиск вершины по имени"))

@export
(defgeneric find-edge    (graph edge-name) (:documentation "Поиск ребра по имени"))

;;(defgeneric connected-nodes (node)         (:documentation "Поиск достижимых вершин"))

@export
(defgeneric nea-from-nodes  (node)         (:documentation "Возвращает хеш-таблицу вершин, с которыми соединена вершина node, в направлении от нее к ним"))

@export
(defgeneric nea-to-nodes    (node)         (:documentation "Возвращает хеш-таблицу вершин, с которыми соединена вершина node, в направлении от них к ней"))

@export
(defgeneric to-graphviz  (obj stream)      (:documentation "Выполняет объекта obj в формат программы graphviz"))

@export
(defgeneric view-graph   (graph &key fpath fname graphviz-prg out-type dpi viewer)
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

@export
@annot.class:export-class
(defclass <node> ()
  ((name    :accessor node-name    :initarg :name  :initform nil :documentation "Имя вершины")
   (owner   :accessor node-owner   :initarg :owner :initform nil :documentation "Владелец вершины объект типа graph")
   (counter :accessor node-counter                 :initform 0   :documentation "Количество, созданных вершин" :allocation :class))
   (:documentation "Вершина графа"))

@export
@annot.class:export-class
(defclass <edge> ()
  ((start :accessor edge-from :initarg :from :initform nil :documentation "Начальная вершина ребра")
   (end   :accessor edge-to   :initarg :to   :initform nil :documentation "Конечная  вершина ребра"))
  (:documentation "Ребро графа"))

@export
@annot.class:export-class
(defclass <graph> ()
  ((nodes :accessor graph-nodes :initform (make-hash-table) :documentation "Хешированная таблица вершин графа")
   (edges :accessor graph-edges :initform (make-hash-table) :documentation "Хешированная таблица ребер графа"))
  (:documentation "Представляет граф, выражающий алгоритм изменения состояния агрегатов во времени"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod initialize-instance :around ((x <node>) &key name (owner nil))
  (call-next-method x
		    :name   name
    		    :owner  owner 
		    :number (node-counter x))
  (when owner (insert-to x owner))
  (incf (node-counter x)))

;;;;;;;;;; print-object ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-object        ((x <node>) s))
(defmethod print-object :after ((x <node>) s)
	   (format s "~S:~S"   (not(null (node-owner x))) (node-name x)))

(defmethod print-object        ((x <edge>) s))
(defmethod print-object :after ((x <edge>) s)
  (format s "(~S->~S)" (edge-from x) (edge-to x)))

(defmethod print-object        ((x <graph>) s))
(defmethod print-object :after ((x <graph>) s)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass <printer-viewer> ()
  ((graphviz-prg :accessor printer-viewer-graphviz-prg :initarg :graphviz-prg :initform :filter-dot
		 :documentation "Программа для вывода")
   (dpi          :accessor printer-viewer-dpi          :initarg :dpi          :initform "300"
		 :documentation "Разрешение принтера")
   (executable   :accessor printer-viewer-executable   :initarg :executable   :initform "C:/Program Files/Internet Explorer/iexplore.exe"
		 :documentation "Путь к выполняемой программе")
   (out-type     :accessor printer-viewer-out-type :initarg :out-type         :initform "pdf"
		 :documentation "Формат файла для вывода"))
  (:documentation "Принтер-просмотрщик"))

(defmethod print-object :after ((pv <printer-viewer>) s)
  (format s "
 graphviz-prg = ~S
 dpi          = ~S
 executable   = ~S
 out-type     = ~S~%"
	  (printer-viewer-graphviz-prg pv)
	  (printer-viewer-dpi pv)
	  (printer-viewer-executable pv)
	  (printer-viewer-out-type  pv)
	  ))

(defclass <pdf-printer-viewer> (<printer-viewer>) ()
    (:documentation "PDF - принтер-просмотрщик"))

(defmethod initialize-instance ((pv <pdf-printer-viewer>)
				&key
				  (graphviz-prg :filter-dot)
				  (dpi "300")
				  (executable
				   (cond
				     ((uiop/os:os-windows-p)
				      (cond
					((probe-file "C:/Program Files (x86)/Adobe/Acrobat Reader DC/Reader/AcroRd32.exe") "C:/Program Files (x86)/Adobe/Acrobat Reader DC/Reader/AcroRd32.exe")
					((probe-file "C:/Program Files/Adobe/Reader 11.0/Reader/AcroRd32.exe") "C:/Program Files/Adobe/Reader 11.0/Reader/AcroRd32.exe")))
				     ((uiop/os:os-unix-p)
				      (cond
					((probe-file "/usr/bin/atril") "/usr/bin/atril")
					((probe-file "/usr/bin/okular") "/usr/bin/okular"))))
				   )
				  (out-type "pdf")
				  )
  (setf (printer-viewer-graphviz-prg pv) graphviz-prg)
  (setf (printer-viewer-dpi          pv) dpi)
  (setf (printer-viewer-executable   pv) executable)
  (setf (printer-viewer-out-type     pv) out-type))

(defclass <svg-printer-viewer> (<printer-viewer>) ()
      (:documentation "SVG - принтер-просмотрщик"))

(defmethod initialize-instance
    ((pv <svg-printer-viewer>)
     &key
       (graphviz-prg :filter-dot)
       (dpi "300")
              (executable
	(cond
	  ((uiop/os:os-windows-p)
	   (cond
	     ((probe-file "C:/Program Files/Internet Explorer/iexplore.exe") "C:/Program Files/Internet Explorer/iexplore.exe")
	     ((probe-file "C:/Program Files/Mozilla Firefox/firefox.exe") "C:/Program Files/Mozilla Firefox/firefox.exe")))
	  ((uiop/os:os-unix-p)
	   (cond
	     ((probe-file "/usr/bin/atril") "/usr/bin/atril")
	     ((probe-file "/usr/bin/atril") "/usr/bin/okular"))))
	)
       (out-type "svg")
       )
  (setf (printer-viewer-graphviz-prg pv) graphviz-prg)
  (setf (printer-viewer-dpi          pv) dpi)
  (setf (printer-viewer-executable   pv) executable)
  (setf (printer-viewer-out-type     pv) out-type))

; (printer-viewer graphviz-prg pv)
; (printer-viewer-dpi          pv)
; (printer-viewer-executable   pv)
; (printer-viewer-out-type     pv)

(defmethod view-graph-new ((g <graph>) (pv <printer-viewer>)
			   &key
			     (fpath *output-path*)
			     (fname  (format nil "graph-~6,'0D" (incf *graph-count*)))
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
			(list (concatenate 'string fpath "/" fname ".gv" "." out-type))
			:wait nil))
  g)

;;;;;;;;;; to-string ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@export
@annot.doc:doc
"@b(Описание:) to-string !!!!!!
"
(defmethod to-string (val) (format nil "~A" val))

@export
@annot.doc:doc
"@b(Описание:) to-string !!!!!!
"
(defmethod to-string ((x <node>)) (format nil "~A" (node-name x)))

@export
@annot.doc:doc
"@b(Описание:) to-string !!!!!!
"
(defmethod to-string ((x <edge>))
  (format nil "~A->~A" (to-string (edge-from x)) (to-string (edge-to x))))

;;;;;;;;;; insert-to ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@export
@annot.doc:doc
"@b(Описание:) insert-to !!!!!!
"
(defmethod insert-to ((n <node>) (g <graph>))
  (setf (gethash n (graph-nodes g)) n
	(node-owner n) g)
  n)

@export
@annot.doc:doc
"@b(Описание:) insert-to ((e <edge>) (g <graph>))!!!!!!
"
(defmethod insert-to ((e <edge>) (g <graph>))
  (setf (gethash e (graph-edges g)) e)
  (setf (node-owner (edge-from e)) g)
  (setf (node-owner (edge-to   e)) g)
  (setf (gethash (edge-from e) (graph-nodes g)) (edge-from e))
  (setf (gethash (edge-to   e) (graph-nodes g)) (edge-to   e))
  e)

;;;;;;;;;; remove-from ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@export
@annot.doc:doc
"@b(Описание:) remove-from ((n <node>) (g <graph> ))!!!!!!
"
(defmethod remove-from ((n <node>) (g <graph> ))
  (let* ((rh (graph-edges g))
	 (rl (hash-table-copy rh)))
    (maphash #'(lambda(key val)
		 val
		 (if (or
		      (eq (edge-from key) n)
		      (eq (edge-to key)   n))
		     (remhash key rh)))
	     rl)
    (if (remhash n (graph-nodes g))
	n)))

@export
@annot.doc:doc
"@b(Описание:) remove-from ((e <edge>) (g <graph> ) )!!!!!!
"
(defmethod remove-from ((e <edge>) (g <graph> ) )
  (if (remhash e (graph-edges g))
	e))

;;;;;;;;;; graph-clear ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@export
@annot.doc:doc
"@b(Описание:) graph-clear ((g <graph>))!!!!!!
"
(defmethod graph-clear ((g <graph>))
  (clrhash (graph-nodes g))
  (clrhash (graph-edges g))
  g)

;;;;;;;;;;  inlet outlet ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@export
@annot.doc:doc
"@b(Описание:) outlet-edges ((n <node>) &aux (g (node-owner n)))!!!!!!
"
(defmethod outlet-edges ((n <node>) &aux (g (node-owner n)))
  (let ((rez-tbl(hash-table-copy (graph-edges g))))
    (maphash
     #'(lambda (key val)
	 val
	 (if (not(eq (edge-from key) n))
	     (remhash  key rez-tbl)))
     (graph-edges g))
    rez-tbl))

@export
@annot.doc:doc
"@b(Описание:) inlet-edges ((n <node>) &aux (g (node-owner n)))!!!!!!
"
(defmethod inlet-edges ((n <node>) &aux (g (node-owner n)))
  (let ((rez-tbl (hash-table-copy (graph-edges g))))
    (maphash
     #'(lambda (key val)
	 val
	 (if (not(eq (edge-to key) n))
	     (remhash  key rez-tbl)))
     (graph-edges g))
    rez-tbl))

;;;;;;;;;; find-* ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@export
@annot.doc:doc
"@b(Описание:) find-node ((g <graph>) (str string))!!!!!!
"
(defmethod find-node ((g <graph>) (str string))
  (let ((v-rez nil))
    (maphash #'(lambda (key val)
		 val
	       (if (string= (to-string key) str)
		   (setf v-rez key)))
	     (graph-nodes g))
    v-rez))

@export
@annot.doc:doc
"@b(Описание:) find-edge ((g <graph>) (str string))!!!!!!
"
(defmethod find-edge ((g <graph>) (str string))
  (let ((e-rez nil))
    (maphash #'(lambda (key val)
		 val
	       (if (string= (to-string key) str)
		   (setf e-rez key)))
	     (graph-edges g))
    e-rez))

;;;; to-graphviz ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@export
@annot.doc:doc
"@b(Описание:) to-graphviz ((n <node>) s)!!!!!!
"
(defmethod to-graphviz ((n <node>) s)
  (format s "~S~%" (to-string n)))

@export
@annot.doc:doc
"@b(Описание:) to-graphviz ((r <edge>) s)!!!!!!
"
(defmethod to-graphviz ((r <edge>) s)
  (format s "~S ~A ~S~%"
	  (to-string (edge-from r))
	  "->"
	  (to-string (edge-to r))))

@annot.doc:doc
"x-preamble записывает преамбулу при выводе графа в gv-файл."
(defun x-preamble (&key (out t) (name "G") (rankdir "LR") (shape "box"))
  (format out "digraph ~A {~%  rankdir=~A~%  node[shape=~A]~%" name rankdir shape))

@annot.doc:doc
"x-preamble записывает постамбулу при выводе графа в gv-файл."
(defun x-postamble (&key (out t)) (format out "~&}~%"))

@export
@annot.doc:doc
"@b(Описание:) to-graphviz ((g <graph>) s)!!!!!!
"
(defmethod to-graphviz ((g <graph>) s)
  (x-preamble :out s)
  (maphash #'(lambda (key val) val (to-graphviz key s)) (graph-nodes g))  
  (maphash #'(lambda (key val) val (to-graphviz key s)) (graph-edges g))  
  (x-postamble :out s))

;;;; make-graph data ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@export
@annot.doc:doc
"@b(Описание:) make-graph возвращает граф с ребрами edges и вершинами вершинами nodes."
(defun make-graph (edges &key nodes)
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
@annot.doc:doc
"graphviz-prg - возвращает путь к программе dot или ее вариациям."
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

@export
@annot.doc:doc
"@b(Описание:)  view-graph 
"
(defmethod view-graph ((g <graph>) 
		       &key
			 (fpath *output-path*)
			 (fname  (format nil "graph-~6,'0D" (incf *graph-count*)))
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
			(list (concatenate 'string fpath "/" fname ".gv" "." out-type))
			:wait nil))
  g)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@export
@annot.doc:doc
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
(defun make-random-graph (&key (node-max-number 100) (edges-number node-max-number))
  (make-graph
   (let ((lst nil))
     (dotimes (i edges-number lst)
       (push (list
	      (format nil "~A" (random node-max-number))
	      (format nil "~A" (random node-max-number)))
	     lst)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@export
@annot.doc:doc
"@b(Описание:) nea-to-nodes возвращает хеш-таблицу вершин, с которыми соединена вершина <node>, в направлении от нее к ним.
"
(defmethod nea-to-nodes  ((n <node>) &aux (ht (make-hash-table)))
  
  (maphash
   #'(lambda (key val)
       val
       (setf (gethash (edge-to key) ht) (edge-to key)))
   (outlet-edges n))
  (print-items ht)
  ht)

@export
@annot.doc:doc
"@b(Описание:) nea-from-nodes
Возвращает хеш-таблицу вершин, с которыми соединена вершина <node>, в направлении от них к ней.
"
(defmethod nea-from-nodes  ((n <node>) &aux (ht (make-hash-table)))
  (maphash
   #'(lambda (key val)
       val
       (setf (gethash (edge-from key) ht) (edge-from key)))
   (inlet-edges n))
  (print-items ht)
  ht)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@export
@annot.doc:doc
"@b(Описание:) connected-nodes ((n <node>) &key (direction :direction-to) &aux (ht (make-hash-table )))!!!!!!
"
(defmethod connected-nodes ((n <node>) &key (direction :direction-to) &aux (ht (make-hash-table )))
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
