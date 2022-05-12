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

(defun for-nodes (l-nodes graph &key direction)
   (let ((func-for-edges (case direction
                          (:forward  #'outlet-edges)
                          (:backward #'inlet-edges)
                          (:both     #'both-edges))))
  (apply #'append
         (mapcar
          #'(lambda (node)
              (mnas-hash-table:keys
               (funcall func-for-edges node graph)))
          (remove-duplicates l-nodes)))))

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

(defun for-edges (edges graph &key direction)
  (let ((func-beg (case direction
                    (:forward  #'tail)
                    (:backward #'head)
                    (:both #'tail)))
        (func-end (case direction
                    (:forward  #'head)
                    (:backward #'tail)
                    (:both #'head))))
               (format t "for-edges ~{~A~^; ~}~%" edges)
               (mapcar
                #'(lambda (edge)
                    (for-edge edge func-beg func-end graph))
                (remove-duplicates edges))))

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
            (list beg-node) graph :direction direction)
           (for-nodes
            (for-edges e-s graph :direction direction)
            graph :direction direction)))
         ((= c-e (hash-table-count ht-e)) graph)
      (setf c-e (hash-table-count ht-e)) ;; предыдущее количество обработанных ребер
      (map nil ;; добавление обработанных ребер в хеш-таблицу
           #'(lambda (edge)
               (setf (gethash edge ht-e) nil))
           e-s))
    graph))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+nil
(progn
  (defparameter *g* (make-graph '(("a" "b") ("a" "b") ("b" "c")("b" "d") ("d" "a") ("c" "a"))))
  (view *g*)
  (path (find-node "a" *g*) (find-node "d" *g*) *g* :direction :forward)
  (mnas-graph/view:view-graph *g*)
  (view *g*))
