;;;; ./src/core/methods/find-node.lisp

(in-package #:mnas-graph)

(defmethod find-node ((g <graph>) (str string))
  "@b(Описание:) find-node ((g <graph>) (str string))!!!!!!
"
  (let ((v-rez nil))
    (maphash #'(lambda (key val)
		 val
	         (if (string= (to-string key) str)
		     (setf v-rez key)))
	     (nodes g))
    v-rez))
