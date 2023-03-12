;;;; ./src/core/methods/name.lisp

(in-package :mnas-graph)

(defmethod name ((edge <edge>))
  "
 @b(Пример использования:)
@begin[lang=lisp](code)
  (let ((graph (mnas-graph:make-graph
              '((\"a\" \"c\") (\"c\" \"d\") (\"c\" \"g\") (\"c\" \"e\")
                (\"e\" \"f\") (\"e\" \"g\") (\"h\" \"j\") (\"b\" \"f\"))
              :nodes '(\"k\"))))
  (name (mnas-graph:find-edge \"c->d\" graph)))
"
  (to-string edge))



