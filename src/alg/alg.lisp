;;; ./src/alg/alg.lisp

(defpackage #:mnas-graph/alg
  (:use #:cl #:mnas-graph)
  (:export path
           )
  (:documentation
   "Пакет @b(mnas-graph/alg) реализует некоторые алгоритмы на графах.
"))

(in-package :mnas-graph/alg)

(defgeneric path (beg-node end-node graph)
  (:documentation
   "@b(Описание:) обобщенная функция @b(path) возвращает список вершин,
  представляющий кратчайший путь из вершины @b(beg-node) до вершины @b(end-node)
  графа @b(graph)."
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun init (beg-node graph)
  "@b(Описание:) функция @b(init) инициализирует значения для каждой из
  вершин графа списком (w n), где w=nil - сумма весов предыдущих
  вершин; n=nil -предыдущая вершина). Для начальной вершины
  @(beg-node) w=0; n=nil.
 "
  (let ((ht-nodes (nodes graph))
        (nodes (mnas-hash-table:keys (nodes graph))))
    (mapcar
     #'(lambda (node)
         (setf (gethash node ht-nodes) '(nil nil)))
     nodes)
    (setf (gethash beg-node ht-nodes)  '(0 nil))))

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
 модифицирует значения хеш-таблицы вершин так, что для значение для
 определенной вершины является списком двух элементов. Первый элемент
 этого списка является числом, представляющим минимальную длину пути,
 найденную к этой вершине от начальной вершины. Второй элемент
 является предыдущей вершиной, для которой найден путь с минимальной
 длиной.
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

(defun back (end-node graph)
  (do* ((n (second (gethash end-node (nodes graph)))
           (second (gethash n (nodes graph))))
        (rez (list end-node)))
       ((null n) rez)
    (push n rez)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod path ((beg-node <node>) (end-node <node>) (graph <graph>))
  (let (;;(ht-nodes (nodes graph))
        ;;(nodes (mnas-hash-table:keys (nodes graph)))
        ;;(l-nodes (list beg-node))
        (ht-e (make-hash-table)) ;; хеш-таблица, встреченнх ребер
        (c-e -1) ;; количество встреченных ребер для предыдущей итерации
        )
    (init beg-node graph)
    #+nil (break ":0001")
    (do* ((n-s (list beg-node) (for-edges e-s graph))
          (e-s (for-nodes n-s graph) (for-nodes n-s graph)))
         ((= c-e (hash-table-count ht-e)) (view graph))
      (setf c-e (hash-table-count ht-e))
      (map nil
           #'(lambda (edge)
               (setf (gethash edge ht-e) nil))
           e-s)
      #+nil(break ":0002")
      )
    (back end-node graph))) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *g* (copy mnas-graph/tests::*g*))

(view *g*)

(path (find-node "a" *g*) (find-node "f" *g*)  *g*)




