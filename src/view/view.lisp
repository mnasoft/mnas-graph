;;;; ./src/view/view.lisp

(defpackage #:mnas-graph/view
  (:use #:cl #:mnas-graph #:mnas-graph/printer-viewer #:mnas-graph/filter)
  (:export view-graph
           view-graph-new
           to-graphviz)
  (:export *graph-count*
           *output-path*
           *viewer-path*
           )
  (:documentation
   " Пакет @b(mnas-graph/view) определяет функции для вывода и визуализации графов.

 Вывод представления графа в файл производится в формате программы
 @link[uri=\"https://graphviz.org\"](graphviz).

 Преобразование графа в пригодный для визуального представления формат
 выполняется при помощи программы
 @link[uri=\"https://graphviz.org\"](graphviz) или ее фильтров укладки.

 В настоящее время доступны только базовые возможности визуализации графов. 
"
   ))

(in-package  #:mnas-graph/view)

(defparameter *graph-count* -1
  " @b(Описание:) глобальная переменная @b(*graph-count*) используется
 для задния имени \(по-умолчанию\) файлу, содержащему граф.")

(defparameter *output-path*
  (cond ((uiop/os:os-windows-p) (namestring (probe-file "~/."))) 
	((uiop/os:os-unix-p) (namestring (probe-file "~/."))))
  "@b(Описание:) глобальная переменная @b(*output-path*) определяет
 каталог для вывода файлов графов \(по-умолчанию.\)")

(defparameter *viewer-path*
  (cond
    ((uiop/os:os-windows-p)
     (cond
       ((probe-file "C:/Program Files (x86)/Adobe/Acrobat Reader DC/Reader/AcroRd32.exe")
        "C:/Program Files (x86)/Adobe/Acrobat Reader DC/Reader/AcroRd32.exe")
       ((probe-file "C:/Program Files/Adobe/Reader 11.0/Reader/AcroRd32.exe")
        "C:/Program Files/Adobe/Reader 11.0/Reader/AcroRd32.exe")
       ((probe-file "C:/Program Files/Adobe/Acrobat DC/Acrobat/Acrobat.exe")
        "C:/Program Files/Adobe/Acrobat DC/Acrobat/Acrobat.exe")
       (t (assert (probe-file "C:/Program Files/Adobe/Acrobat DC/Acrobat/Acrobat.exe")))))
    ((uiop/os:os-unix-p)
     (cond
       ((probe-file "/usr/bin/xdg-open") "/usr/bin/xdg-open")
       ((probe-file "/usr/bin/atril") "/usr/bin/atril")
       ((probe-file "/usr/bin/atril") "/usr/bin/okular")
       )))
  "Программа просмотрщик Графа")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric to-graphviz (obj stream)
  (:documentation
   "@b(Описание:) обобщенная функция @b(to-graphviz) выполняет объекта
   @b(obj) в поток @b(stream) в формате программы @link[uri=\"https://graphviz.org\"](Graphviz)." ))

(defgeneric view-graph (graph &key fpath fname graphviz-prg out-type dpi viewer)
  (:documentation
   "@b(Описание:) обобщенная функция @b(view-graph) выполняет визуализацию графа @b(graph).

 @b(Переменые:)
@begin(list)
 @item(fpath - каталог для вывода результатов работы программы;)
 @item(fname - имя gv-файла;)
 @item(out-type - тип выходного файла;)
 @item(dpi - количество точек на дюйм;)
 @item(viewer - программа для просмотра графа;)
 @item(graphviz-prg - программа для генерации графа.)
@end(list)

 graphviz-prg может принимать одно из следующих значений:
@begin(list)
 @item(:filter-dot;)
 @item(:filter-neato;)
 @item(:filter-twopi;)
 @item(:filter-circo;)
 @item(:filter-fdp;)
 @item(:filter-sfdp;)
 @item(:filter-patchwork.)
@end(list)
"))

;;;; to-graphviz ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod to-graphviz ((n <node>) s)
  " @b(Описание:) метод @b(to-graphviz) выполняет вывод вершины графа
 @b(n) в поток @(s)."
  (format s "~A~%" n))

(defmethod to-graphviz ((r <edge>) s)
  " @b(Описание:) метод @b(to-graphviz) выполняет вывод ребра графа
 @b(r) в поток @(s)."
  (format s "~S ~A ~S~%"
	  (name (beg-node r))
	  "->"
	  (name (end-node r))))

(defun x-preamble (&key (out t) (name "G") (rankdir "LR") (shape "box"))
  "@b(Описание:) функция @b(x-preamble) выводит преамбулу при выводе
 графа в поток @b(out) в формате gv-файла."
  (format out "digraph ~A {~%  rankdir=~A~% node[shape=~A]~%" name rankdir shape))

(defun x-postamble (&key (out t))
  "@b(Описание:) функция @b(x-postamble) записывает постамбулу при
 выводе графа в поток @b(out) в формате gv-файла."
  (format out "~&}~%"))

(defmethod to-graphviz ((g <graph>) s)
  " @b(Описание:) метод @b(to-graphviz) выполняет вывод графа
 @b(g) в поток @(s)."
  (x-preamble :out s)
  (maphash #'(lambda (key val) val (to-graphviz key s)) (nodes g))  
  (maphash #'(lambda (key val) val (to-graphviz key s)) (edges g))  
  (x-postamble :out s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun graphviz-prg (key)
  " @b(Описание:) функция @b(graphviz-prg) возвращает путь к программе
 dot или ее вариациям."
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
  " @b(Описание:) метод @b(view-graph) выполняет визализацию графа @b(g). 

 Выполнение метода проходит в три этапа:
@begin(enum)
@item(Вывод графа @b(g) в gv-файл в формате программы
 @link[uri=\"https://graphviz.org\"](Graphviz).)
@item(Преобразование gv-файла в формат @b(out-type) при помощи программы
 @link[uri=\"https://graphviz.org\"](Graphviz).)
@item(Визуализация результата вывода просмотрщиком, заданным параметром @b(viewer).)
@end(enum)"
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
