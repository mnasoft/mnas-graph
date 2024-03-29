;;; ./src/alg/temp.lisp

(in-package :mnas-graph/alg)

(defun init (beg-node graph)
          (let ((ht-nodes (nodes graph))
                (nodes (mnas-hash-table:keys (nodes graph))))
            (mapcar
             #'(lambda (node)
                 (setf (gethash node ht-nodes) '(nil nil)))
             nodes)
            (setf (gethash beg-node ht-nodes)  '(0 nil))))

(defun for-nodes (lst-ht-nodes graph
                  &key (direction :both))
  (case direction
    (:forward
     (loop :for ht-nodes :in lst-ht-nodes
             :collect
             (let ((ht-ed (make-hash-table)))
               (loop :for node :being :the :hash-keys :in ht-nodes :do
                 (loop :for edge :being :the :hash-keys :in (outlet-edges node graph) :do
                   (setf (gethash edge ht-ed) nil)))
               ht-ed)))
    (:backward
     (loop :for ht-nodes :in lst-ht-nodes
           :collect
           (let ((ht-ed (make-hash-table)))
             (loop :for node :being :the :hash-keys :in ht-nodes :do
               (loop :for edge :being :the :hash-keys :in (inlet-edges node graph) :do
                 (setf (gethash edge ht-ed) nil)))
             ht-ed)))
    (:both
     (loop :for ht-nodes :in lst-ht-nodes
           :for f :in (list #'outlet-edges #'inlet-edges)
           :collect
           (let ((ht-ed (make-hash-table)))
             (loop :for node :being :the :hash-keys :in ht-nodes :do
               (loop :for edge :being :the :hash-keys :in (funcall f node graph) :do
                 (setf (gethash edge ht-ed) nil)))
             ht-ed)))))

(defun for-edge (edge beg-func end-func graph) 
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

(defun for-edges (lst-ht-edges graph &key direction)
  (format t "for-edges ~{~A~^; ~}~%"
          (apply #'append (mapcar #'mnas-hash-table:keys lst-ht-edges)))
  (case direction
    (:forward
     (loop :for ht-edges :in lst-ht-edges
             :collect
             (let ((ht-nd (make-hash-table)))
               (loop :for edge :being :the :hash-keys :in ht-edges :do
                 (setf (gethash (for-edge edge #'tail #'head graph) ht-nd) nil))
               ht-nd)))
    (:backward
     (loop :for ht-edges :in lst-ht-edges
             :collect
             (let ((ht-nd (make-hash-table)))
               (loop :for edge :being :the :hash-keys :in ht-edges :do
                 (setf (gethash (for-edge edge #'head #'tail graph) ht-nd) nil))
               ht-nd)))
    (:both
     (loop :for ht-edges :in lst-ht-edges
           :for i :in '(0 1)
             :collect
             (let ((ht-nd (make-hash-table)))
               (loop :for edge :being :the :hash-keys :in ht-edges :do
                 (when (= i 0)
                   (setf (gethash (for-edge edge #'tail #'head graph) ht-nd) nil))
                 (when (= i 1)
                   (setf (gethash (for-edge edge #'head #'tail graph) ht-nd) nil)))
               ht-nd)))))

(defmethod init-distance-graph ((beg-node <node>) (graph <graph>)
                                &key (direction :both)
                                  #+nil(direction :forward)
                                  #+nil(direction :backward))
  (when (not (nth-value 1 (gethash beg-node (nodes graph))))
    (error "The node=~S is not into ~%graph=~S " beg-node graph))
  (let ((ht-e (make-hash-table))      ; хеш-таблица, встреченных ребер
        (c-e -1) ; количество встреченных ребер для предыдущей итерации
        )
    (init beg-node graph)
    (do* ((e-s
           (for-nodes
            (let ((n-ht (make-hash-table)))
              (setf (gethash beg-node n-ht) nil)
              (list n-ht))
            graph :direction direction)
           (for-nodes
            (for-edges e-s graph :direction direction)
            graph :direction direction)))
         ((= c-e (hash-table-count ht-e)) graph)
      (setf c-e (hash-table-count ht-e)) ;; предыдущее количество обработанных ребер
      (loop :for i :in e-s :do
        (loop :for edge :being :the :hash-keys :in i :do
          (setf (gethash edge ht-e) nil))))
    graph))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+nil
(progn
  (defparameter *g* (make-graph '(("a" "b") ("a" "b") ("b" "c")("b" "d") ("d" "a") ("c" "a"))))
  
  (path (find-node "c" *g*) (find-node "d" *g*) *g* :direction :forward)
  (path (find-node "d" *g*) (find-node "c" *g*) *g* :direction :backward)
  
  (path (find-node "d" *g*) (find-node "c" *g*) *g* :direction :both)
  
  (mnas-graph/view:view-graph *g*)
  (view *g*)
)

 
