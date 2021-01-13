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
           )
  (:export view-graph
           make-random-graph
           to-graphviz
           make-graph
           )
  (:export *output-path*
           *viewer-path*
           *filter-dot*
           *filter-neato*
           *filter-circo*
           *filter-fdp*
           *filter-sfdp*
           *filter-twopi*
           *filter-patchwork*)
  (:export demo-1
           demo-2
           demo-3
           demo-4
           demo-5
           )
  (:documentation
   " Проект mnas-graph определяет базовые функции для создания
 структуры данных типа
 @link[uri='https://ru.wikipedia.org/wiki/Граф_(математика)'](Граф).

Проект определяет следующие основные классы: @begin(list)
@item(@ref[id=class-node](<node>) - вершина графа;)
@item(@ref[id=class-edge](<edge>) - ребро графа;)
@item(@ref[id=class-graph](<graph>) - граф.)  @end(list)


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
@end(code)

и отображения через graphviz."))

;;;; (declaim (optimize (compilation-speed 0) (debug 3) (safety 0) (space 0) (speed 0)))
;;;; (declaim (optimize (space 0) (compilation-speed 0)  (speed 0) (safety 3) (debug 3)))
;;;; (setf sb-impl::*default-external-format* :utf8)

(in-package #:mnas-graph)

(defparameter *graph-count* -1
  "Счетчик созданных вершин, ребер, графов")

(export '*output-path*)
(defparameter *output-path*
  (cond ((uiop/os:os-windows-p) (namestring (probe-file "~/."))) 
	((uiop/os:os-unix-p) (namestring (probe-file "~/."))))
  "Каталог для вывода файлов Графа по-умолчанию.")

(export '*viewer-path*)
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

(export '*filter-dot* )
(defparameter *filter-dot* (make-filter-prg-path "dot")
"dot       - filter for drawing directed graphs"
)

(export '*filter-neato* )
(defparameter *filter-neato* (make-filter-prg-path "neato")
"neato     - filter for drawing undirected graphs"
)

(export '*filter-twopi* )
(defparameter *filter-twopi* (make-filter-prg-path "twopi")
"twopi     - filter for radial layouts of graphs"
)
(export '*filter-circo* )
(defparameter *filter-circo* (make-filter-prg-path "circo")
"circo     - filter for circular layout of graphs"
)
(export '*filter-fdp* )
(defparameter *filter-fdp* (make-filter-prg-path "fdp")
"fdp       - filter for drawing undirected graphs"
)
(export '*filter-sfdp* )
(defparameter *filter-sfdp* (make-filter-prg-path "sfdp")
"sfdp      - filter for drawing large undirected graphs"
)
(export '*filter-patchwork* )
(defparameter *filter-patchwork* (make-filter-prg-path "patchwork")
"patchwork - filter for tree maps"
)
;;;; generics ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric to-string    (obj)
  (:documentation "Выполняет перобразование объекта в строку"))

(defgeneric insert-to    (obj container)
  (:documentation "Добавляет obj в container"))

(defgeneric remove-from  (obj container)
  (:documentation "Добавляет obj в container"))

(defgeneric inlet-nodes  (graph)           (:documentation "Возвращает хеш-таблицу конечных вершин (вершин-стока)"))

(defgeneric outlet-nodes (graph)           (:documentation "Возвращает хеш-таблицу начальных вершин (веншин-иточников)"))

(defgeneric inlet-edges  (node)            (:documentation "Возвращает хеш-таблицу начальных ребер (истоков)"))

(defgeneric outlet-edges (node)
  (:documentation
   " Возвращает хеш-таблицу конечных ребер (устий)"))

(defgeneric find-node    (graph <node>-name) (:documentation "Поиск вершины по имени"))

(defgeneric find-edge    (graph edge-name) (:documentation "Поиск ребра по имени"))

;;(defgeneric connected-nodes (node)         (:documentation "Поиск достижимых вершин"))

(defgeneric nea-from-nodes  (node)         (:documentation "Возвращает хеш-таблицу вершин, с которыми соединена вершина node, в направлении от нее к ним"))

(defgeneric nea-to-nodes    (node)         (:documentation "Возвращает хеш-таблицу вершин, с которыми соединена вершина node, в направлении от них к ней"))

(defgeneric to-graphviz  (obj stream)      (:documentation "Выполняет объекта obj в формат программы graphviz"))

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

(defclass <node> ()
  ((name    :accessor <node>-name  :initarg :name  :initform nil :documentation "Имя вершины")
   (owner   :accessor <node>-owner :initarg :owner :initform nil :documentation "Владелец вершины объект типа graph")
   (counter :accessor <node>-counter                 :initform 0   :documentation "Количество, созданных вершин" :allocation :class))
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

(export 'to-string )
(defmethod to-string (val)
"@b(Описание:) to-string !!!!!!
"
  (format nil "~A" val))

(export 'to-string )
(defmethod to-string ((x <node>))
"@b(Описание:) to-string !!!!!!
"
 (format nil "~A" (<node>-name x)))
(export 'to-string )

(defmethod to-string ((x <edge>))
"@b(Описание:) to-string !!!!!!
"
  (format nil "~A->~A" (to-string (<edge>-from x)) (to-string (<edge>-to x))))
;;;;;;;;;; insert-to ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export 'insert-to )
(defmethod insert-to ((n <node>) (g <graph>))
"@b(Описание:) insert-to !!!!!!
"
  (setf (gethash n (<graph>-nodes g)) n
	(<node>-owner n) g)
  n)

(export 'insert-to )
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
(export 'remove-from )
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

(export 'remove-from )
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

(export 'find-node )
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

(export 'find-edge )
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

;;;; to-graphviz ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export 'to-graphviz )
(defmethod to-graphviz ((n <node>) s)
"@b(Описание:) to-graphviz ((n <node>) s)!!!!!!
"
  (format s "~S~%" (to-string n)))
(export 'to-graphviz )
(defmethod to-graphviz ((r <edge>) s)
"@b(Описание:) to-graphviz ((r <edge>) s)!!!!!!
"
  (format s "~S ~A ~S~%"
	  (to-string (<edge>-from r))
	  "->"
	  (to-string (<edge>-to r))))

(defun x-preamble (&key (out t) (name "G") (rankdir "LR") (shape "box"))
"x-preamble записывает преамбулу при выводе графа в gv-файл."
  (format out "digraph ~A {~%  rankdir=~A~%  node[shape=~A]~%" name rankdir shape))
(defun x-postamble (&key (out t))
"x-preamble записывает постамбулу при выводе графа в gv-файл."
 (format out "~&}~%"))
(export 'to-graphviz )
(defmethod to-graphviz ((g <graph>) s)
"@b(Описание:) to-graphviz ((g <graph>) s)!!!!!!
"
  (x-preamble :out s)
  (maphash #'(lambda (key val) val (to-graphviz key s)) (<graph>-nodes g))  
  (maphash #'(lambda (key val) val (to-graphviz key s)) (<graph>-edges g))  
  (x-postamble :out s))

;;;; make-graph data ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export 'make-graph )
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
(defun graphviz-prg (key)
"graphviz-prg - возвращает путь к программе dot или ее вариациям."
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

(export 'view-graph )
(defmethod view-graph ((g <graph>) 
		       &key
			 (fpath *output-path*)
			 (fname  (format nil "graph-~6,'0D" (incf *graph-count*)))
			 (graphviz-prg :filter-dot)
			 (out-type "pdf")
			 (dpi "300")
			 (viewer *viewer-path*))
"@b(Описание:)  view-graph 
"
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

(export 'make-random-graph )
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
