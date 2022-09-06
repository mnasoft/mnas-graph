;;;; ./src/core/methods/clear.lisp

(in-package #:mnas-graph)

(defmethod clear ((graph <graph>))
  "
 @b(Пример использования:)
@begin[lang=lisp](code)
  (let ((graph (mnas-graph:make-random-graph)))
    (format t \"~A~%~%~A\" graph (mnas-graph:clear graph)))
@end(code)
"
  (clrhash (nodes graph))
  (clrhash (edges graph))
  (clrhash (ht-node-names graph))
  (clrhash (ht-edge-names graph))
  graph)


(defmethod hierarchy-node-names (gr &aux (graph (mnas-graph:copy gr)))
  (let ((rez nil))
    (labels
        ((outlet (graph)
           (let ((lst (mnas-hash-table:keys
                       (mnas-graph:outlet-nodes graph))))
             (map nil
                  #'(lambda (el)
                      (mnas-graph:remove-from el graph))
                  lst)
             (setf rez (append rez lst))
             lst))
         (isolated (graph)
           (let ((lst (mnas-hash-table:keys
                       (mnas-graph:isolated-nodes graph))))
             (map nil
                  #'(lambda (el)
                      (mnas-graph:remove-from el graph))
                  lst)
             (setf rez (append rez lst))
             lst)))
      (do* ((isolated  (isolated graph) (isolated graph))
            (outlet (outlet graph) (outlet graph)))
          ((mnas-graph:empty graph) (mapcar #'mnas-graph:name rez))))))
