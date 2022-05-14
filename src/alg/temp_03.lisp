;;; ./src/alg/temp.lisp

(in-package :mnas-graph/alg)

;;;; Алгоритм Дейкстры
;;;; Инициализировать сеть нод


(defun init (beg-node graph)
  (let ((ht-nodes (nodes graph))
        (nodes (mnas-hash-table:keys (nodes graph))))
    (mapcar
     #'(lambda (node)
         (setf (gethash node ht-nodes) '(nil nil)))
     nodes)
    (setf (gethash beg-node ht-nodes)  '(0 nil))))

(defun select-nearest (graph)
  (nodes graph)
  )

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



