;;;; testing.lisp

(in-package :mnas-graph)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;

(:export 
	   vertex
	   vertex-number
	   vertex-state)

(defclass vertex (<node>)
  ((number  :accessor vertex-number  :initarg :number                :documentation "Номер вершины")
   (state   :accessor vertex-state   :initarg :state  :initform nil  :documentation "Ссылка на состояние вершины"))
  (:documentation "Вершина графа поддерживающая номер и состояние"))


(defmethod initialize-instance :around ((x vertex) &key node-name vertex-state)
  (call-next-method x
		    :name           node-name 
		    :vertex-number (<node>-counter x)
		    :vertex-state  vertex-state)
  (incf (<node>-counter x)))

(defmethod print-object :after ((x vertex) s)
    (call-next-method x)
	   (format s "(~S ~S)~%"
		   (vertex-number x)
		   (vertex-state  x)))

(defmethod to-string ((x vertex)) (format nil "~A:~A" (name x) (vertex-number x)))


(make-instance 'vertex :name "NNN")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *pv-svg* (make-instance '<svg-printer-viewer>))

(defparameter *pv-pdf* (make-instance '<pdf-printer-viewer>))

