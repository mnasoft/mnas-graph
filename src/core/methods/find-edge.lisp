;;;; ./src/core/methods/find-edge.lisp]]

(in-package #:mnas-graph)

(defmethod find-edge ((str string) (g <graph>))
  "
 @b(Пример использования:)
@begin[lang=lisp](code)
  (let ((graph (mnas-graph:make-graph
                '((\"a\" \"c\") (\"c\" \"d\") (\"c\" \"g\") (\"c\" \"e\")
                  (\"e\" \"f\") (\"e\" \"g\") (\"h\" \"j\") (\"b\" \"f\"))
                :nodes '(\"k\"))))
    (mnas-graph:find-edge \"a->c\" graph))
@end(code)
"
  (let ((e-rez nil))
    (maphash #'(lambda (key val)
		 val
	         (if (string= (to-string key) str)
		     (setf e-rez key)))
	     (edges g))
    e-rez))
