;;; ./src/alg/temp.lisp

(in-package :mnas-graph/alg)

;;;; Алгоритм Дейкстры
;;;; Инициализировать сеть нод

(defun deijkstra (beg-node end-node graph)
  (let ((ht-finished-nodes (make-hash-table)))
    (labels
        ((init (beg-node graph)
           (let ((max-length
                   (1+ (loop :for edge :being :the :hash-keys :in (edges graph)
                             :summing (weight edge)))))
             (loop :for node :being :the :hash-keys :in (nodes graph) :do
               (if (eq node beg-node)
                   (setf (gethash node (nodes graph)) 0)
                   (setf (gethash node (nodes graph)) max-length)))))
         (select-nearest-node (graph)
           (let ((lst-node
                   (loop :for node :being :the :hash-keys :in (nodes graph)
                         :unless (nth-value 1 (gethash node ht-finished-nodes))
                           :collect (list node (gethash node (nodes graph))))))
             (first
              (first
               (sort lst-node #'< :key #'(lambda (el) (second el)))))))
         (for-node (c-node graph)
           (loop :for edge :being :the :hash-keys :in (outlet-edges c-node graph) :do
             (setf (gethash (head edge) (nodes graph))
                   (min (+ (gethash (tail edge) (nodes graph))
                           (weight edge))
                        (gethash (head edge) (nodes graph)))))
           (setf (gethash c-node ht-finished-nodes)
                 (gethash c-node (nodes graph)))))
      (init beg-node graph)
      (do ((c-node (select-nearest-node graph) (select-nearest-node graph)))
          ((eq c-node end-node) (view graph))
        (for-node c-node graph)))))

(deijkstra (find-node "a" *g*) (find-node "h" *g*) *g*)

(init (find-node "a" *g*) *g*)

(for-node (select-nearest-node *g*) *g*)
(view *g*)

(view *g*)

(for-node (find-node "a" *g*) *g*)

(gethash (find-node "b" *g*) (nodes *g*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+nil
(progn
  (defparameter *g* (make-graph '(("a" "b") ("a" "b") ("b" "c")("b" "d") ("d" "a") ("c" "a"))))
  
  (path (find-node "c" *g*) (find-node "d" *g*) *g* :direction :forward)
  (path (find-node "d" *g*) (find-node "c" *g*) *g* :direction :backward)
  
  (path (find-node "d" *g*) (find-node "c" *g*) *g* :direction :both)
  
  (mnas-graph/view:view-graph *g*)
  (view *g*)

  (defparameter *g* (make-graph '(("a" "b"  1)
                                  ("b" "c"  1)
                                  ("c" "d" 1)
                                  ("d" "e" 1)
                                  ("a" "e" 10)
                                  ("e" "g" 1)
                                  ("g" "h" 1))))
  
  (path (find-node "a" *g*) (find-node "h" *g*) *g* :direction :both)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (ql:quickload "cl-containers")

  (defparameter *q*
    (make-instance
     'cl-containers:priority-queue-on-container
     :initial-contents '(2 4 1 5 9 3 5)))

  (cl-containers:first-element *q*)

  (cl-containers:delete-first *q*)

  (cl-containers:insert-item *q* 1.5)

  (cl-containers:size *q*)

  (cl-containers:empty-p *q*))



