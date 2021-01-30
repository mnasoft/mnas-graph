;;;; ./src/view/view.lisp

(defpackage #:mnas-graph/view
  (:use #:cl #:mnas-graph #:mnas-graph/printer-viewer #:mnas-graph/filter)
  (:export view-graph
           view-graph-new
           to-graphviz)
  (:export *graph-count*
           *output-path*
           *viewer-path*
           ))

(in-package  #:mnas-graph/view)

(defparameter *graph-count* -1
  "Счетчик созданных вершин, ребер, графов")

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
