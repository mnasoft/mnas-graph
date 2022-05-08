;;;; ./src/core/methods/find-node.lisp

(in-package #:mnas-graph)

(defmethod find-node ((str string) (g <graph>))
  "
 @b(Пример использования:)
@begin[lang=lisp](code)
  (let ((graph (mnas-graph:make-graph
                '((\"a\" \"c\") (\"c\" \"d\") (\"c\" \"g\") (\"c\" \"e\")
                  (\"e\" \"f\") (\"e\" \"g\") (\"h\" \"j\") (\"b\" \"f\"))
                :nodes '(\"k\"))))
    (mnas-graph:find-node \"c\" graph))
@end(code)
"
  (let ((v-rez nil))
    (maphash #'(lambda (key val)
		 val
	         (if (string= (to-string key) str)
		     (setf v-rez key)))
	     (nodes g))
    v-rez))
