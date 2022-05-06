;;;; ./src/core/methods/find-edge.lisp]]

(in-package #:mnas-graph)

(defmethod find-edge ((str string) (g <graph>))
  "@b(Описание:) find-edge ((g <graph>) (str string))!!!!!!
"
  (let ((e-rez nil))
    (maphash #'(lambda (key val)
		 val
	         (if (string= (to-string key) str)
		     (setf e-rez key)))
	     (edges g))
    e-rez))
