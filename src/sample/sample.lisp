;;; ./src/sample/sample.lisp

(defpackage #:mnas-graph/sample
  (:use #:cl #:mnas-graph)
  (:export path
           )
  (:documentation
   "Пакет @b(mnas-graph/sample) реализует некоторые алгоритмы на графах.
"))

(in-package :mnas-graph/sample)

(defun make-linear-graph (n)
  (let ((graph (make-instance '<graph>)))
    (loop :for i :from 1 :below n  
          :for j :from 2 :do
            (insert-to
             (make-instance
              '<edge>
              :tail (make-instance '<node> :name (format nil "~A" i))
              :head (make-instance '<node> :name (format nil "~A" j)))
             graph))
    graph))

(defun make-rectangular-graph (n m)
  (let ((graph (make-instance '<graph>)))
    (loop :for i :from 1 :to n :do
      (loop :for j :from 1 :to m :do
        (insert-to (make-instance '<node> :name (format nil "~A;~A" i j))
                   graph)))
    (loop :for i :from 1 :to n :for i+1 :from 2 :do
      (loop :for j :from 1 :below m :for j+1 :from 2 :do
        (insert-to
         (make-instance '<edge>
                        :tail (find-node (format nil "~A;~A" i j) graph)
                        :head (find-node (format nil "~A;~A" i j+1) graph))
         graph)))
    (loop :for i :from 1 :below n :for i+1 :from 2 :do
      (loop :for j :from 1 :to m :for j+1 :from 2 :do
        (insert-to
         (make-instance '<edge>
                        :tail (find-node (format nil "~A;~A" i j) graph)
                        :head (find-node (format nil "~A;~A" i+1 j) graph))
         graph)))
    graph))

#+nil
(progn
 (mnas-graph/view:view-graph (make-linear-graph 5) :graphviz-prg :filter-neato)
 (mnas-graph/view:view-graph (make-rectangular-graph 20 32) :graphviz-prg :filter-neato)
)



