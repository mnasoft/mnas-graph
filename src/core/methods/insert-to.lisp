;;;; ./src/core/methods/insert-to.lisp

(in-package #:mnas-graph)

(defmethod insert-to ((n <node>) (graph <graph>))
"
  @b(Пример использования:)
@begin[lang=lisp](code)
  (let ((graph (mnas-graph:make-graph
                '((\"a\" \"c\") (\"c\" \"d\") (\"c\" \"g\") (\"c\" \"e\")
                  (\"e\" \"f\") (\"e\" \"g\") (\"h\" \"j\") (\"b\" \"f\"))
                :nodes '(\"k\"))))
    (insert-to (make-instance 'mnas-graph:<node> :name \"l\") graph))
"
  (setf (gethash n (nodes graph)) n
	(owner n) graph)
  n)

(defmethod insert-to ((e <edge>) (graph <graph>))
"
  @b(Пример использования:)
@begin[lang=lisp](code)
  (let ((graph (mnas-graph:make-graph
                '((\"a\" \"c\") (\"c\" \"d\") (\"c\" \"g\") (\"c\" \"e\")
                  (\"e\" \"f\") (\"e\" \"g\") (\"h\" \"j\") (\"b\" \"f\"))
                :nodes '(\"k\"))))
    (insert-to
     (make-instance 'mnas-graph:<edge>
                    :tail (mnas-graph:find-node \"a\" graph)
                    :head (mnas-graph:find-node \"g\" graph))
     graph))
"
  (setf (gethash e (edges graph)) e)
  (setf (owner (tail e)) graph)
  (setf (owner (head   e)) graph)
  (setf (gethash (tail e) (nodes graph)) (tail e))
  (setf (gethash (head e) (nodes graph)) (head e))
  e)

(defmethod insert-to ((name string) (graph <graph>)
                      &aux (node (make-instance '<node> :name name)))
  "
  @b(Пример использования:)
@begin[lang=lisp](code)
  (let ((graph (mnas-graph:make-graph
                '((\"a\" \"c\") (\"c\" \"d\") (\"c\" \"g\") (\"c\" \"e\")
                  (\"e\" \"f\") (\"e\" \"g\") (\"h\" \"j\") (\"b\" \"f\"))
                :nodes '(\"k\"))))
    (insert-to \"l\" graph))
"
  (insert-to node graph))
