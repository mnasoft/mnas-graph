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

(defun for-nodes (ht-nodes graph
                  &key (direction :both)
                  &aux (ht-ed (make-hash-table)))
  (case direction
    (:forward
     (progn
       (loop :for node :being :the :hash-keys :in ht-nodes :do
         (loop :for edge :being :the :hash-keys :in (outlet-edges node graph) :do
           (setf (gethash edge ht-ed) nil))))
     ht-ed)
    (:backward
     (progn
       (loop :for node :being :the :hash-keys :in ht-nodes :do
         (loop :for edge :being :the :hash-keys :in (inlet-edges node graph) :do
           (setf (gethash edge ht-ed) nil)))
       ht-ed))
    (:both
     (progn
       (loop :for node :being :the :hash-keys :in ht-nodes :do
         (loop :for edge :being :the :hash-keys :in (both-edges node graph) :do
           (setf (gethash edge ht-ed) nil)))
       ht-ed))))

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



(defun for-edges (ht-edges graph &key direction
                  &aux (ht-nd (make-hash-table)))
  (format t "for-edges ~{~A~^; ~}~%" (mnas-hash-table:keys ht-edges))
  (case direction
    (:forward
     (progn
       (loop :for edge :being :the :hash-keys :in ht-edges :do
         (setf (gethash (for-edge edge #'tail #'head graph) ht-nd) nil))
       ht-nd))
    (:backward
     (progn
       (loop :for edge :being :the :hash-keys :in ht-edges :do
         (setf (gethash (for-edge edge #'head #'tail graph) ht-nd) nil))
       ht-nd))
    #+nil
    (:both
     (progn
       (loop :for edge :being :the :hash-keys :in ht-edges :do
         (setf (gethash (for-edge edge #'tail #'head graph) ht-nd) nil)
         (setf (gethash (for-edge edge #'head #'tail graph) ht-nd) nil))
       ht-nd))
    (:both
     (progn
       (loop :for edge :being :the :hash-keys :in ht-edges :do
         (setf (gethash (for-edge edge #'tail #'head graph) ht-nd) nil)
         (setf (gethash (for-edge edge #'head #'tail graph) ht-nd) nil))
       ht-nd))))

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
              n-ht)
             graph :direction direction)
           (for-nodes
            (for-edges e-s graph :direction direction)
            graph :direction direction)))
         ((= c-e (hash-table-count ht-e)) graph)
      (setf c-e (hash-table-count ht-e)) ;; предыдущее количество обработанных ребер
      (loop :for edge :being :the :hash-keys :in e-s :do
        (setf (gethash edge ht-e) nil)))
    graph))

(map nil ;; добавление обработанных ребер в хеш-таблицу
     #'(lambda (edge)
         (setf (gethash edge ht-e) nil))
     e-s)

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

(init (find-node "d" *g*) *g*)

(defun for-edge-both (edge graph) 
  (let ((beg (tail edge)) 
        (end (head edge))) 
    (let ((lst-end (gethash end (nodes graph)))
          (lst-beg (gethash beg (nodes graph))))
      (list lst-beg lst-end)
      
      (progn
        (cond
          ((null (first lst-end))
           (setf (gethash end (nodes graph))
                 (list
                  (+ (weight edge) (first lst-beg))
                  beg))
           end)
        
          ((null (first lst-beg))
           (setf (gethash beg (nodes graph))
                 (list
                  (+ (weight edge) (first lst-end))
                  end)))
          ((and (numberp (first lst-end)) 
                (< (+ (weight edge) (first lst-beg))
                   (first lst-end))) 
           (setf (gethash end (nodes graph))
                 (list
                  (+ (weight edge) (first lst-beg)) 
                  beg))
           end)) 

        ))))

(defparameter *e-l*
  (mnas-hash-table:keys
   (for-nodes (let ((n-ht (make-hash-table)))
                (setf (gethash (find-node "d" *g*) n-ht) nil)
                n-ht)
              *g*
              :direction :both)))

(for-edge-both (nth 0 *e-l*) *g*) 
