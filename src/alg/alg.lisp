;;; ./src/alg/alg.lisp

(defpackage #:mnas-graph/alg
  (:use #:cl #:mnas-graph)
  (:export path
           )
  (:documentation
   "Пакет @b(mnas-graph/alg) реализует некоторые алгоритмы на графах.
"))

(in-package :mnas-graph/alg)

(defgeneric path (beg-node end-node graph &key direction)
  (:documentation
   "@b(Описание:) обобщенная функция @b(path) возвращает список вершин,
  представляющий кратчайший путь из вершины @b(beg-node) до вершины @b(end-node)
  графа @b(graph)."
   ))

(defgeneric init-distance-graph (beg-node graph &key direction)
  (:documentation
   "@b(Описание:) обобщенная функция @b(init-distance-graph) возвращает
 граф @b(graph), модифицируя значения, связанные с вершинами данными
 для вычисления кратчайшего пути от определенной вершины до любой из
 вершин графа."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun view (graph)
  (list
   (mnas-hash-table:to-list (nodes graph))
   (mnas-hash-table:to-list (edges graph))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod init-distance-graph ((beg-node <node>) (graph <graph>)
                                &key (direction :both)  #+nil(direction :forward) #+nil(direction :backward))
  (let ((func-for-edges (case direction
                          (:forward  #'outlet-edges)
                          (:backward #'inlet-edges)
                          (:both     #'both-edges)))
        (func-beg (case direction
                    (:forward  #'tail)
                    (:backward #'head)
                    (:both #'tail)))
        (func-end (case direction
                    (:forward  #'head)
                    (:backward #'tail)
                    (:both #'head))))
    (labels
        ((
          init ()
          "@b(Описание:) функция @b(init) инициализирует значения для каждой из
  вершин графа списком (w n), где w=nil - сумма весов предыдущих
  вершин; n=nil - предыдущая вершина). Для начальной вершины
  @(beg-node) w=0; n=nil."
          (let ((ht-nodes (nodes graph))
                (nodes (mnas-hash-table:keys (nodes graph))))
            (mapcar
             #'(lambda (node)
                 (setf (gethash node ht-nodes) '(nil nil)))
             nodes)
            (setf (gethash beg-node ht-nodes)  '(0 nil))))
         (
          for-nodes (l-nodes)
           "@b(Описание:) функция @b(for-nodes) для списка @b(l-nodes) вершин
           графа @b(graph) возвращает список:

 - исходящих ребер если func-for-edges = outlet-edges;
 - входящих ребер если func-for-edges  = inlet-edges;
 - исходящих и входящих ребер если func-for-edges  = both-edges;
 ."
          (apply #'append
                 (mapcar
                  #'(lambda (node)
                      (mnas-hash-table:keys
                       (funcall func-for-edges node graph)))
                  (remove-duplicates l-nodes))))
         (
          for-edge (edge beg-func end-func) 
          (let ((end (funcall end-func edge)) 
                (beg (funcall beg-func edge))) 
            (let ((lst-end (gethash end (nodes graph)))
                  (lst-beg (gethash beg (nodes graph))))
              (cond
                ((null (first lst-end))
                 (setf (gethash end (nodes graph))
                       (list
                        (+ (weight edge) (first lst-beg))
                        beg))) 
                ((and (numberp (first lst-end)) 
                      (< (+ (weight edge) (first lst-beg))
                         (first lst-end))) 
                 (setf (gethash end (nodes graph))
                       (list
                        (+ (weight edge) (first lst-beg)) 
                        beg)))) 
              end)))
         (for-edges (edges)
           (format t "for-edges ~{~A~^; ~}~%" edges)
           "@b(Описание:) функция @b(for-edges) возвращает список вершин,
 расположенных в:
 - головах ребер @b(edges), если
 - хвостах ребер @b(edges), если
 - головах и хвостах ребер @b(edges).

 В качестве побочного эффекта модифицирует значения хеш-таблицы вершин
 графа @b(graph) так, что для значение для определенной вершины
 является списком двух элементов. Первый элемент этого списка является
 числом, представляющим минимальную длину пути, найденную к этой
 вершине от начальной вершины. Второй элемент является предыдущей
 вершиной, для которой найден путь с минимальной длиной."
           (mapcar
            #'(lambda (edge)
                (for-edge edge func-beg func-end))
            (remove-duplicates edges))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (when (not (nth-value 1 (gethash beg-node (nodes graph))))
        (error "The node=~S is not into ~%graph=~S " beg-node graph))
      (let ((ht-e (make-hash-table))  ; хеш-таблица, встреченных ребер
            (c-e -1) ; количество встреченных ребер для предыдущей итерации
            )
        (init)
        (do* ((e-s (for-nodes (list beg-node)) (for-nodes (for-edges e-s))))
             ((= c-e (hash-table-count ht-e)) (view graph))
          (setf c-e (hash-table-count ht-e)) ;; предыдущее количество обработанных ребер
          (map nil ;; добавление обработанных ребер в хеш-таблицу
               #'(lambda (edge)
                   (setf (gethash edge ht-e) nil))
               e-s))
        graph))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod path ((beg-node <node>) (end-node <node>) (graph <graph>)
                 &key (direction :both) #+nil(direction :forward) #+nil(direction :backward) )
;;; Проверка на принадлежность начальной @b(beg-node) и конечной
;;; @b(end-node) вершины графу @b(graph)
  (when (or (not (nth-value 1 (gethash beg-node (nodes graph))))
            (not (nth-value 1 (gethash end-node (nodes graph)))))
    (return-from path nil))
  (labels
      ((back (end-node graph)
         (do* ((n (second (gethash end-node (nodes graph)))
                  (second (gethash n (nodes graph))))
               (rez (list end-node)))
              ((null n) rez)
           (push n rez))))
    (let ((rez (back
                end-node
                #+nil(init-distance-graph beg-node graph)
                #+nil(init-distance-graph-inlet beg-node graph)
                (init-distance-graph beg-node graph :direction direction))))
;;; Проверка на то, что путь является правильным - начинается в
;;; вершине beg-node.
      (if (eq beg-node (first rez))
          (values rez (first (gethash end-node (nodes graph))))
          (values nil nil)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+nil
(progn
  (defparameter *g* (copy mnas-graph/tests::*g*))
  (defparameter *g* (make-graph '(("a" "b") ("a" "b") ("b" "c")("b" "d") ("d" "a") ("c" "a"))))
  (view *g*)
  (path (find-node "a" *g*) (find-node "d" *g*) *g* :direction :forward)
  
  (path (find-node "a" *g*) (find-node "f" *g*) *g* :direction :forward)
  (path (find-node "a" *g*) (find-node "b" *g*) *g* :direction :backward)
  
  (path (find-node "g" *g*) (find-node "a" *g*) *g* :direction :backward)
  (path (find-node "g" *g*) (find-node "a" *g*) *g* :direction :both)

  (init-distance-graph (find-node "a" *g*) *g*)
  (mnas-graph/view:view-graph *g*)
  (view *g*))
