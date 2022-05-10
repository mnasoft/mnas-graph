;;; ./src/alg/alg.lisp

(defpackage #:mnas-graph/alg
  (:use #:cl #:mnas-graph)
  (:export path
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
      ((g  (make-instance 'mnas-graph:<graph>))
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
    (mnas-graph/view:view-graph g))
@end(code)"))

(in-package :mnas-graph/alg)

(defgeneric path (beg-node end-node graph)
  (:documentation
   "@b(Описание:) обобщенная функция @b(path)."
   ))

(defmethod path ((beg-node <node>) (end-node <node>) (graph <graph>))
  (let ((ht-nodes (nodes graph))
        (nodes (mnas-hash-table:keys (nodes graph)))
        (l-nodes (list beg-node))
        (ht-n-seens (make-hash-table))
        (before 0)
        (past   1))
    (block init
      #+nil "
 1. Инициализировать значения для каждой из вершин графа списком
 (вес предыдущая-вершина), где вес - nil; предыдущая-вершина - nil."
      (mapcar
       #'(lambda (node)
           (setf (gethash node ht-nodes) '(nil nil)))
       nodes)
    (setf (gethash beg-node ht-nodes)   '(0 nil)))
    (setf (gethash beg-node ht-n-seens) nil)
    (block for-nodes
      #+nil "
 2. Найти исходящие ребра."
      (apply #'append
             (mapcar
              #'(lambda (node)
                  (mnas-hash-table:keys (outlet-edges node graph)))
              l-nodes)))
    ))

(defun init (beg-node graph)
 "@b(Описание:) функция @b(init) инициализирует значения для каждой из
  вершин графа списком (вес предыдущая-вершина), где вес - nil;
  предыдущая-вершина - nil.  Начальная вершина beg-node
  инициализируется списком (вес предыдущая-вершина), где вес - 0;
  предыдущая-вершина - nil.
 "
  (let ((ht-nodes (nodes graph))
        (nodes (mnas-hash-table:keys (nodes graph))))
    (mapcar
     #'(lambda (node)
         (setf (gethash node ht-nodes) '(nil nil)))
     nodes)
    (setf (gethash beg-node ht-nodes)   '(0 nil))))

(defun view (graph)
  (list
   (mnas-hash-table:to-list (nodes graph))
   (mnas-hash-table:to-list (edges graph))))

(defun for-nodes (l-nodes graph)
  "@b(Описание:) функция @b(for-nodes) возвращает список исходящих ребер
 для вершин графа @b(graph), заданных списком @b(l-nodes).
"
  (apply #'append
         (mapcar
          #'(lambda (node)
              (mnas-hash-table:keys (outlet-edges node graph)))
          l-nodes)))

(defun for-edges (edges graph)
    "@b(Описание:) функция @b(for-edges) возвращает список вершин,
 расположенных в головах ребер @b(edges). В качестве побочного эффекта
 модифицирует значения хеш-таблицы вершин так, что значение
 представляет список, первый элемент которого является числом,
 представляющим минимальный путь до

3. Для каждого исходящего ребра для головной вершины определить сумму
сумму значения хвостовой вершины и вес ребра; если веc nil записать
для головной вершины зачение в список (вес предыдущая-вершина).  если
вес - число, меньшее записанного nil.
"
  (mapcar
   #'(lambda (edge)
       (let* ((head (head edge))
              (tail (tail edge))
              (lst-head (gethash head (nodes graph)))
              (lst-tail (gethash tail (nodes graph))))
         #+nil (break "head = ~S~%tail = ~S~%lst-head = ~S~%lst-tail = ~S" head tail lst-head lst-tail)
         (cond
           ((null (first lst-head))
            #+nil (break ":001")
            (setf (gethash head (nodes graph))
                  (list
                   (+ (weight edge) (first lst-tail))
                   tail)))
           ((and (numberp (first lst-head))
                 (< (+ (weight edge) (first lst-tail))
                    (first lst-head)))
            #+nil(break ":002")
            (setf (gethash head (nodes graph))
                  (list
                   (+ (weight edge) (first lst-tail))
                   tail))))
         head))
   edges))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *g* (copy mnas-graph/tests::*g*))

(init (find-node "c" *g*) *g*)

(defparameter *n* (list (find-node "c" *g*)))

(defparameter *e* (for-nodes *n* *g*))

(defparameter *n* (for-edges *e* *g*))

(view *g*)

