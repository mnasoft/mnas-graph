;;;; tests/main.lisp

(in-package :mnas-graph/tests)

(def-suite main
  :description "Мастер-набор всех тестов проекта mnas-graph."
  :in all)

(in-suite main)

(def-fixture fix-graph-g ()
  (let ((g (mnas-graph:make-graph
            '(("a" "c") ("c" "d") ("c" "g") ("c" "e")
              ("e" "f") ("e" "g") ("h" "j") ("b" "f"))
            :nodes
            '("k"))))
    (&body)))

(def-test test-nodes ()
  (with-fixture fix-graph-g ()
    (is-true
     (equal (mnas-graph:ids (mnas-graph:nodes g))
            #+nil (mnas-graph:ids (mnas-graph:nodes *g*))
            '("a" "b" "c" "d" "e" "f" "g" "h" "j" "k")))))

(def-test test-node-names ()
  (with-fixture fix-graph-g ()
    (is-true
     (equal (mnas-graph:node-names g)
            #+nil (mnas-graph:node-names *g*)
            '("a" "b" "c" "d" "e" "f" "g" "h" "j" "k")))))

(def-test test-edges ()
  (with-fixture fix-graph-g ()
    (is-true
     (equal (mnas-graph:ids (mnas-graph:edges g))
            #+nil (mnas-graph:ids (mnas-graph:edges *g*))
            '("a->c" "b->f" "c->d" "c->e" "c->g" "e->f" "e->g" "h->j")))))

(def-test test-edge-names ()
  (with-fixture fix-graph-g ()
    (is-true
     (equal (mnas-graph:edge-names g)
            #+nil (mnas-graph:edge-names *g*)
            '("a->c" "b->f" "c->d" "c->e" "c->g" "e->f" "e->g" "h->j")))))

(def-test test-inlet-edges ()
  (with-fixture fix-graph-g ()
    (loop :for (node rez) :in
          #+nil (loop :for node :in (mnas-graph:ids
                                     (mnas-graph:nodes *g*))
                      :collect (list node (mnas-graph:ids
                                           (mnas-graph:inlet-edges
                                            (mnas-graph:find-node *g* node)))))
          '(("a" NIL) ("b" NIL) ("c" ("a->c")) ("d" ("c->d")) ("e" ("c->e"))
            ("f" ("b->f" "e->f")) ("g" ("c->g" "e->g")) ("h" NIL) ("j" ("h->j")) ("k" NIL))
          :do (is-true
               (equal (mnas-graph:ids
                       (mnas-graph:inlet-edges
                        (mnas-graph:find-node g node)))
                      rez)))))

(def-test test-outlet-edges ()
  (with-fixture fix-graph-g ()
    (loop :for (node rez) :in
          #+nil (loop :for node :in (mnas-graph:ids
                                     (mnas-graph:nodes *g*))
                      :collect
                      (list node (mnas-graph:ids
                                  (mnas-graph:outlet-edges
                                   (mnas-graph:find-node *g* node)))))
          '(("a" ("a->c")) ("b" ("b->f")) ("c" ("c->d" "c->e" "c->g")) ("d" NIL)
            ("e" ("e->f" "e->g")) ("f" NIL) ("g" NIL) ("h" ("h->j")) ("j" NIL) ("k" NIL))
          :do (is-true
               (equal (mnas-graph:ids
                       (mnas-graph:outlet-edges
                        (mnas-graph:find-node g node)))
                      rez)))))

(def-test test-both-edges ()
  (with-fixture fix-graph-g ()
    (loop :for (node rez) :in
          #+nil (loop :for node :in (mnas-graph:ids
                                     (mnas-graph:nodes *g*))
                      :collect
                      (list node (mnas-graph:ids
                                  (mnas-graph:both-edges
                                   (mnas-graph:find-node *g* node)))))
          '(("a" ("a->c")) ("b" ("b->f")) ("c" ("a->c" "c->d" "c->e" "c->g"))
            ("d" ("c->d")) ("e" ("c->e" "e->f" "e->g")) ("f" ("b->f" "e->f"))
            ("g" ("c->g" "e->g")) ("h" ("h->j")) ("j" ("h->j")) ("k" NIL))
          :do (is-true
               (equal (mnas-graph:ids
                       (mnas-graph:both-edges
                        (mnas-graph:find-node g node)))
                      rez)))))

(def-test test-isolated-p ()
  (with-fixture fix-graph-g ()
    (loop :for (node rez) :in
          #+nil (loop :for node :in (mnas-graph:ids
                                     (mnas-graph:nodes *g*))
                      :collect (list
                                node
                                (mnas-graph:isolated-p
                                 (mnas-graph:find-node *g* node))))
          '(("a" NIL) ("b" NIL) ("c" NIL) ("d" NIL) ("e" NIL) ("f" NIL) ("g" NIL)
            ("h" NIL) ("j" NIL) ("k" T))
          :do (is-true
               (equal (mnas-graph:isolated-p
                       (mnas-graph:find-node g node))
                      rez)))))

(def-test test-inlet-p ()
  (with-fixture fix-graph-g ()
    (loop :for (node rez) :in
          #+nil (loop :for node :in (mnas-graph:ids
                                     (mnas-graph:nodes *g*))
                      :collect (list node
                                     (mnas-graph:inlet-p
                                      (mnas-graph:find-node *g* node))))
          '(("a" NIL) ("b" NIL) ("c" NIL) ("d" T) ("e" NIL)
            ("f" T) ("g" T) ("h" NIL) ("j" T) ("k" NIL))
          :do (is-true
               (equal (mnas-graph:inlet-p
                       (mnas-graph:find-node g node))
                      rez)))))

(def-test test-outlet-p ()
  (with-fixture fix-graph-g ()
    (loop :for (node rez) :in
          #+nil(loop :for node :in (mnas-graph:ids
                                    (mnas-graph:nodes *g*))
                     :collect (list node (mnas-graph:outlet-p
                                          (mnas-graph:find-node *g* node))))
          '(("a" T) ("b" T) ("c" NIL) ("d" NIL) ("e" NIL) ("f" NIL) ("g" NIL) ("h" T)
            ("j" NIL) ("k" NIL))
          :do (is-true
               (equal (mnas-graph:outlet-p
                       (mnas-graph:find-node g node))
                      rez)))))

(def-test test-inlet-nodes ()
  (with-fixture fix-graph-g ()
    (is-true
     (equal (mnas-graph:ids (mnas-graph:inlet-nodes g))
            #+nil (mnas-graph:ids (mnas-graph::inlet-nodes *g*))
            '("d" "f" "g" "j")))))

(def-test test-outlet-nodes ()
  (with-fixture fix-graph-g ()
    (is-true
     (equal (mnas-graph:ids
             (mnas-graph:outlet-nodes g))
            #+nil (mnas-graph:ids
                   (mnas-graph::outlet-nodes *g*))
            '("a" "b" "h")))))

(def-test test-isolated-nodes ()
  (with-fixture fix-graph-g ()
    (is-true
     (equal (mnas-graph:ids
             (mnas-graph:isolated-nodes g))
            #+nil (mnas-graph:ids
                   (mnas-graph:isolated-nodes *g*))
            '("k")))))


(def-test test-find-inlet-nodes ()
  (with-fixture fix-graph-g ()
    (loop :for (node rez) :in
          #+nil (loop :for node :in (mnas-graph:ids
                                     (mnas-graph:nodes *g*))
                      :collect (list node
                                     (mnas-graph:ids
                                      (mnas-graph:find-inlet-nodes
                                       (mnas-graph:find-node *g* node)))))
          
          '(("a" NIL) ("b" NIL) ("c" ("a")) ("d" ("c")) ("e" ("c")) ("f" ("b" "e"))
            ("g" ("c" "e")) ("h" NIL) ("j" ("h")) ("k" NIL))
          :do (is-true
               (equal (mnas-graph:ids
                       (mnas-graph:find-inlet-nodes
                        (mnas-graph:find-node g node)))
                      rez)))))

(def-test test-find-inlet-nodes ()
  (with-fixture fix-graph-g ()
    (loop :for (node rez) :in
          #+nil (loop :for node :in (mnas-graph:ids
                                     (mnas-graph:nodes *g*))
                      :collect (list node
                                     (mnas-graph:ids
                                      (mnas-graph:find-inlet-nodes
                                       (mnas-graph:find-node *g* node)))))
          
          '(("a" NIL) ("b" NIL) ("c" ("a")) ("d" ("c")) ("e" ("c")) ("f" ("b" "e"))
            ("g" ("c" "e")) ("h" NIL) ("j" ("h")) ("k" NIL))
          :do (is-true
               (equal (mnas-graph:ids
                       (mnas-graph:find-inlet-nodes
                        (mnas-graph:find-node g node)))
                      rez)))))

(def-test test-find-outlet-nodes ()
  (with-fixture fix-graph-g ()
    (loop :for (node rez) :in
          #+nil (loop :for node :in (mnas-graph:ids
                                     (mnas-graph:nodes *g*))
                      :collect (list node
                                     (mnas-graph:ids
                                      (mnas-graph:find-outlet-nodes
                                       (mnas-graph:find-node *g* node)))))
          '(("a" ("c")) ("b" ("f")) ("c" ("d" "e" "g")) ("d" NIL) ("e" ("f" "g"))
            ("f" NIL) ("g" NIL) ("h" ("j")) ("j" NIL) ("k" NIL))
          :do (is-true
               (equal (mnas-graph:ids
                       (mnas-graph:find-outlet-nodes
                       (mnas-graph:find-node g node)))
                      rez)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-test test-connected-nodes ()
  (with-fixture fix-graph-g ()
    (is-true
     (equal (mnas-graph:ids
             (mnas-graph:connected-nodes
              (mnas-graph:find-node g "c") :direction :backward))
            #+nil
            (mnas-graph:ids
             (mnas-graph:connected-nodes
              (mnas-graph:find-node *g* "c") :direction :backward))
            '("a" "c")))
    (is-true
     (equal (mnas-graph:ids
             (mnas-graph:connected-nodes
              (mnas-graph:find-node g "c") :direction :forward))
            #+nil (mnas-graph:ids
                   (mnas-graph:connected-nodes
                    (mnas-graph:find-node *g* "c") :direction :forward))
            '("c" "d" "e" "f" "g")))))

;;(mnas-graph/view:view-graph *g* :graphviz-prg :filter-neato)

#+nil
(

(defparameter *g*
  (mnas-graph:make-graph '(("a" "c") ("c" "d") ("c" "g") ("c" "e")
                           ("e" "f") ("e" "g") ("h" "j") ("b" "f"))
                           :nodes
                           '("k")))

(mnas-hash-table:to-list (mnas-graph:edges *g*))

;(mnas-graph:to-string
(mnas-graph:insert-to   "m" *g*) 
(mnas-graph:remove-from "e" *g*)  

;(mnas-graph:outlet-nodes

(ql:quickload :mnas-hash-table)

(mnas-graph:node-names *g*)


outlet-edges
find-node
find-edge
connected-nodes
(mnas-graph:find-inlet-nodes (mnas-graph:find-node *g* "c"))
(mnas-graph:find-outlet-nodes (mnas-graph:find-node *g* "c"))


(mnas-graph/view:view-graph *g*)
)
