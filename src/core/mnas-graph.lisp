;;;; .src/core/mnas-graph.lisp

(defpackage #:mnas-graph
  (:use #:cl
        #:mnas-hash-table)
  (:export color <color> 
           ;;shape <shape>
           )
  (:export <node> 
           <edge>
           <graph>)
  (:export name
           owner
           <node>-counter ;; Подлежит удалению
           )
  (:export tail
           head)
  (:export nodes
           edges)
  ;; <node> 
  (:export connected-nodes
           nea-from-nodes
           nea-to-nodes
           inlet-edges
           outlet-edges
           )
  ;; <node> & <edge>
  (:export to-string
           )
  (:export to-nodes
           from-nodes)
  ;; <graph>
  (:export clear
           )
  (:export inlet-nodes
           outlet-nodes
           )
  (:export insert-to
           remove-from
           )
  (:export find-node
           find-edge
           )
  (:export count-nodes
           count-edges
           )
  (:export make-graph
           make-random-graph
           )
  (:export name-edges
           )
  (:documentation
   " Пакет @b(mnas-graph) определяет базовые функции для создания
 структуры данных типа
 @link[uri=\"https://en.wikipedia.org/wiki/Graph_(discrete_mathematics)\"](Graph)
 и ее отображения через @link[uri=\"https://graphviz.org/\"](graphviz).

 Пакет определяет следующие основные классы: 
@begin(list)
@item(@ref[id=class-node](<node>) - вершина графа;)
@item(@ref[id=class-edge](<edge>) - ребро графа;)
@item(@ref[id=class-graph](<graph>) - граф.)  
@end(list)

 @b(Пример использования:)
@begin[lang=lisp](code)
  (let*
      ((g (make-instance 'mnas-graph:<graph>))
       (v1 (make-instance 'mnas-graph:<node> :owner g :name \"v1\"))
       (v2 (make-instance 'mnas-graph:<node> :owner g :name \"v2\"))
       (v3 (make-instance 'mnas-graph:<node> :owner g :name \"v3\"))
       (r1 (make-instance 'mnas-graph:<edge> :tail v1 :head v2))
       (r2 (make-instance 'mnas-graph:<edge> :tail v2 :head v3))
       (r3 (make-instance 'mnas-graph:<edge> :tail v3 :head v1)))
    (mnas-graph:insert-to v1 g)
    (mnas-graph:insert-to v2 g)
    (mnas-graph:insert-to v3 g)
    (mnas-graph:insert-to r1 g)
    (mnas-graph:insert-to r2 g)
    (mnas-graph:insert-to r3 g)
    (mnas-graph:view-graph g))
@end(code)"))

;;;; (declaim (optimize (compilation-speed 0) (debug 3) (safety 0) (space 0) (speed 0)))
;;;; (declaim (optimize (space 0) (compilation-speed 0)  (speed 0) (safety 3) (debug 3)))
;;;; (setf sb-impl::*default-external-format* :utf8)

(in-package #:mnas-graph)

;;;; make-graph data ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-graph (edges &key nodes)
  "@b(Описание:) функция @b(make-graph) возвращает граф с ребрами
 @b(edges) и вершинами вершинами @b(nodes).
 
 @b(Пример использования:)
@begin[lang=lisp](code)
  (mnas-graph/view:view-graph
   (make-graph '((\"a\" \"c\") (\"b\" \"c\") (\"c\" \"d\")
                 (\"c\" \"g\") (\"c\" \"e\") (\"e\" \"f\")
                 (\"e\" \"g\") (\"h\" \"j\") (\"b\" \"f\"))
               :nodes
               '(\"k\")))
@end(code)
"
  (let ((g (make-instance '<graph>))
	(vs (remove-duplicates (append (apply #'append edges) nodes) :test #'equal)))
    (mapc #'(lambda (v) (insert-to (make-instance '<node> :name v) g)) vs)
    (mapc #'(lambda (el)
	      (insert-to
	       (make-instance '<edge>
			      :tail (find-node g (first el))
			      :head (find-node g (second el)))
	       g))
	  edges)
    g))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-random-graph (&key (node-max-number 100) (edges-number node-max-number))
  "@b(Описание:) функция @b(make-random-graph) возвращает случайный граф
 с количеством ребер равным @b(edges-number) и количеством вершин не
 превышающим @b(node-max-number).

@b(Пример использования:)
@begin[lang=lisp](code)
  (mnas-graph/view:view-graph
   (make-random-graph :node-max-number 20 :edges-number 10))
@end(code)
"
  (make-graph
   (let ((lst nil))
     (dotimes (i edges-number lst)
       (push (list
	      (format nil "~A" (random node-max-number))
	      (format nil "~A" (random node-max-number)))
	     lst)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





