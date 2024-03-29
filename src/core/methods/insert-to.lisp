;;;; ./src/core/methods/insert-to.lisp

(in-package :mnas-graph)

(defmethod insert-to ((node <node>) (graph <graph>))
  "
  @b(Пример использования:)
@begin[lang=lisp](code)
  (let ((graph (mnas-graph:make-graph
                '((\"a\" \"c\") (\"c\" \"d\") (\"c\" \"g\") (\"c\" \"e\")
                  (\"e\" \"f\") (\"e\" \"g\") (\"h\" \"j\") (\"b\" \"f\"))
                :nodes '(\"k\"))))
    (insert-to (make-instance 'mnas-graph:<node> :name \"l\") graph))
"
  (setf (gethash node (nodes graph)) node)
  (setf (owner node) graph)
  (setf (gethash (name node) (ht-node-names graph)) node)
  node)

(defmethod insert-to ((edge <edge>) (graph <graph>))
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
  (setf (gethash edge (edges graph)) nil) 
  
  (setf (owner (tail edge)) graph)
  (setf (owner (head edge)) graph)
  
  (setf (gethash (tail edge) (nodes graph)) nil)
  (setf (gethash (head edge) (nodes graph)) nil)

  (setf (gethash edge (ht-outlet-edges (tail edge))) nil)
  (setf (gethash edge (ht-inlet-edges  (head edge))) nil)

  (setf (gethash (name edge) (ht-edge-names graph)) edge)
  
  edge)

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
