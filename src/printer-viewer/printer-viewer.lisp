;;;; ./src/printer-viewer/printer-viewer.lisp

(defpackage :mnas-graph/printer-viewer
  (:use #:cl) ;;;; #:mnas-graph
  (:export <printer-viewer>
           <pdf-printer-viewer>
           <svg-printer-viewer>))

(in-package :mnas-graph/printer-viewer)

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
					((probe-file "/usr/bin/okular") "/usr/bin/okular")))))
				  (out-type "pdf"))
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
