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
                                &key (direction :both) #+nil(direction
                                :forward) #+nil(direction :backward))
  "@b(Описание:) метод @b(init-distance-graph) применим только для ребер
 с одинаковыми положительными весами.

 @b(Локльные функции:)
@begin(enum)

 @item( @b(Описание:) локальная функция @b(init) инициализирует
значения для каждой из вершин графа списком (WEIGHT NODE), где:

@begin(list)
 @item(WEIGHT - сумма весов предыдущих вершин;)
 @item(NODE - предыдущая вершина.)
@end(list)

 Значения (WEIGHT NODE) первоначально устанавливаются:
@begin(list)
 @item(для начальной вершины @(beg-node) WEIGHT=0; NODE=nil;)
 @item(для остальных вершин WEIGHT=nil; NODE=nil.)
@end(list))

@item(
 @b(Описание:) локальная функция @b(for-edge) выполняет модификацию
 данных значений вершины графа @b(graph), связанной с ребром @b(edge)
 функцией @b(end-func). Данные вершин модфицируются так, что

@begin(list)
 @item(WEIGHT - становится равным текущей минимальной длине пути от
       начальной вершины;)
 @item(NODE - предыдущая вершина, для которой найден текущей
       кратчайший путь;)
@end(list)

 @b(Переменые:)
@begin(list)
 @item(edge - ребро графа;)
 @item(beg-func - функция, задающая начальную вершину ребра, для
       которой определен текущий кратчайший путь;)
 @item(end-func - функция, задающая конечную вершину ребра, для
       которой определяется текущий кратчайший путь.)
@end(list)
)

@item(
@b(Описание:) функция @b(for-nodes) для хеш-таблицы @b(ht-nodes)
 вершин графа @b(graph) возвращает список, состоящий из двух
 хеш-таблиц, содержащих:
@begin(list)
 @item(первая - исхоящие ребра;)
 @item(вторая - входящие ребра.)
@end(list)
)

@item(
 @b(Описание:) функция @b(for-edges) возвращает хеш-таблицу вершин,
 расположенных далее по направлению поиска:
@begin(list)
 @item(головах ребер @b(edges), если )
 @item(хвостах ребер @b(edges), если )
 @item(головах и хвостах ребер @b(edges) )
@end(list)
 
 @b(Переменые:)
@begin(list)
 @item(lst-ht-edges - список, состоящий из двух хеш-таблиц, которые
       содержат ребра. В первой хеш-таблице содержатся исходящие
       ребра. Во второй хеш-таблице содержатся входящие ребра.)

@end(list)

 В качестве побочного эффекта модифицирует значения хеш-таблицы вершин
 графа @b(graph) так, что для значение для определенной вершины
 является списком двух элементов. Первый элемент этого списка является
 числом, представляющим минимальную длину пути, найденную к этой
 вершине от начальной вершины. Второй элемент является предыдущей
 вершиной, для которой найден путь с минимальной длиной.

)
@end(enum)
"
  (labels
      ((init ()
         (let ((ht-nodes (nodes graph)))
           (loop :for node :being :the :hash-keys :in ht-nodes :do
             (setf (gethash node ht-nodes)    '(nil nil)))
           (setf (gethash beg-node ht-nodes)  '(0 nil))))
       (for-edge (edge beg-func end-func) 
         (let ((end (funcall end-func edge)) 
               (beg (funcall beg-func edge))) 
           (let ((lst-end (gethash end (nodes graph)))
                 (lst-beg (gethash beg (nodes graph))))
             (cond
               ((null (first lst-end))
                (setf (gethash end (nodes graph))
                      (list (+ (weight edge) (first lst-beg)) beg))) 
               ((and (numberp (first lst-end)) 
                     (< (+ (weight edge) (first lst-beg))
                        (first lst-end))) 
                (setf (gethash end (nodes graph))
                      (list (+ (weight edge) (first lst-beg)) beg)))) 
             end)))
       (for-nodes (ht-nodes) 
         (case direction
           (:forward
            (let ((ht-ed (make-hash-table)))
              (loop :for node :being :the :hash-keys :in ht-nodes :do
                (loop :for edge :being :the :hash-keys :in (outlet-edges node graph) :do
                  (setf (gethash edge ht-ed) nil)))
              (list ht-ed (make-hash-table))))
           (:backward
            (let ((ht-ed (make-hash-table)))
              (loop :for node :being :the :hash-keys :in ht-nodes :do
                (loop :for edge :being :the :hash-keys :in (inlet-edges node graph) :do
                  (setf (gethash edge ht-ed) nil)))
              (list (make-hash-table) ht-ed)))
           (:both
            (loop :for f :in (list #'outlet-edges #'inlet-edges)
                  :collect
                  (let ((ht-ed (make-hash-table)))
                    (loop :for node :being :the :hash-keys :in ht-nodes :do
                      (loop :for edge :being :the :hash-keys :in (funcall f node graph) :do
                        (setf (gethash edge ht-ed) nil)))
                    ht-ed)))))
       (for-edges (lst-ht-edges) 
         #+nil(format t "for-edges ~{~A~^; ~}~%"
                      (apply #'append (mapcar #'mnas-hash-table:keys lst-ht-edges)))
         (case direction
           (:forward
            (let ((ht-nd (make-hash-table)))
              (loop :for edge :being :the :hash-keys :in (first lst-ht-edges) :do
                (setf (gethash (for-edge edge #'tail #'head) ht-nd) nil))
              ht-nd))
           (:backward
            (let ((ht-nd (make-hash-table)))
              (loop :for edge :being :the :hash-keys :in (second lst-ht-edges) :do
                (setf (gethash (for-edge edge #'head #'tail) ht-nd) nil))
              ht-nd))
           (:both
            (let ((ht-nd (make-hash-table)))
              (loop :for edge :being :the :hash-keys :in (first lst-ht-edges) :do
                (setf (gethash (for-edge edge #'tail #'head) ht-nd) nil))
              (loop :for edge :being :the :hash-keys :in (first lst-ht-edges) :do
                (setf (gethash (for-edge edge #'head #'tail) ht-nd) nil))
              ht-nd))))
       )
    (when (not (nth-value 1 (gethash beg-node (nodes graph))))
      (error "The node=~S is not into ~%graph=~S " beg-node graph))
    (let ((ht-e (make-hash-table))    ; хеш-таблица, встреченных ребер
          (c-e -1) ; количество встреченных ребер для предыдущей итерации
          )
      (init) 
      (do* ((e-s
             (for-nodes
              (let ((n-ht (make-hash-table)))
                (setf (gethash beg-node n-ht) nil)
                n-ht))
             (for-nodes (for-edges e-s))))
           ((= c-e (hash-table-count ht-e)) graph)
        (setf c-e (hash-table-count ht-e)) ;; предыдущее количество обработанных ребер
        (loop :for i :in e-s :do
          (loop :for edge :being :the :hash-keys :in i :do
            (setf (gethash edge ht-e) nil))))
      graph)))

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
  (mnas-graph/view:view-graph *g* :graphviz-prg :filter-sfdp ) ;; 
  (view *g*)

  (defparameter *g* (make-graph '(("1" "2"  7)
                                  ("1" "3"  9)
                                  ("1" "6" 14)
                                  ("2" "3" 10)
                                  ("2" "4" 15)
                                  ("3" "4" 11)
                                  ("3" "6"  2)
                                  ("4" "5"  6)
                                  ("5" "6"  9))))
  
  (path (find-node "1" *g*) (find-node "2" *g*) *g* :direction :both)  ; => ("1" [ ] "2" [ ]), 7
  (path (find-node "1" *g*) (find-node "3" *g*) *g* :direction :both) ; => ("1" [ ] "3" [ ]), 9
  (path (find-node "1" *g*) (find-node "4" *g*) *g* :direction :both) ; => ("1" [ ] "3" [ ] "4" [ ]), 20
  (path (find-node "1" *g*) (find-node "5" *g*) *g* :direction :both) ; => ("1" [ ] "3" [ ] "6" [ ] "5" [ ]), 20
  (path (find-node "1" *g*) (find-node "6" *g*) *g* :direction :both) ; => ("1" [ ] "3" [ ] "6" [ ]), 11

  (defparameter *g* (make-graph '(("1" "2"  7)
                                  ("1" "3"  9)
                                  ("1" "6" 14)
                                  ("2" "3" 10)
                                  ("2" "4" 15)
                                  ("3" "4" 11)
                                  ("3" "6"  2)
                                  ("4" "5"  6)
                                  ("5" "6"  9))))
  
    (defparameter *g* (make-graph '(("a" "b"  1)
                                    ("b" "c"  1)
                                    ("c" "d" 1)
                                    ("d" "e" 1)
                                    ("a" "e" 10)
                                    ("e" "g" 1)
                                    ("g" "h" 1))))
    (path (find-node "a" *g*) (find-node "h" *g*) *g* :direction :both)
  )


