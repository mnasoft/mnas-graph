;;;; tests/main.lisp

(in-package :mnas-graph/tests)

(def-suite main
  :description "Мастер-набор всех тестов проекта mnas-graph."
  :in all)

(in-suite main)

(def-fixture fix-graph-g ()
  (let ((g (mnas-graph:make-graph
            '(("a" "c") ("b" "c") ("c" "d")
              ("c" "g") ("c" "e") ("e" "f")
              ("e" "g") ("h" "j") ("b" "f"))
            :nodes
            '("k"))))
    (&body)))

(def-test test-nodes ()
  (with-fixture fix-graph-g ()
    (is-true
     (equal (mnas-graph:ids (mnas-graph:nodes g))
            '("a" "b" "c" "d" "e" "f" "g" "h" "j" "k")))))

(def-test test-node-names ()
  (with-fixture fix-graph-g ()
    (is-true
     (equal (mnas-graph:node-names *g* )
            '("a" "b" "c" "d" "e" "f" "g" "h" "j" "k")))))
(def-test test-edges ()
  (with-fixture fix-graph-g ()
    (is-true
     (equal (mnas-graph:ids (mnas-graph:edges g))
            '("a->c" "b->c" "b->f" "c->d" "c->e" "c->g" "e->f" "e->g" "h->j")))))

(def-test test-edge-names ()
  (with-fixture fix-graph-g ()
    (is-true
     (equal (mnas-graph:edge-names g)
            '("a->c" "b->c" "b->f" "c->d" "c->e" "c->g" "e->f" "e->g" "h->j")))))

(def-test test-inlet-edges ()
  "
  (loop :for node :in (mnas-graph:ids
                       (mnas-graph:nodes *g*))
        :collect (list
                  node
                  (mnas-graph:ids
                   (mnas-graph:inlet-edges
                    (mnas-graph:find-node *g* node)))))
"
  (with-fixture fix-graph-g ()
    (loop :for (node rez) :in
          '(("a" NIL) ("b" NIL)
            ("c" ("a->c" "b->c"))
            ("d" ("c->d")) ("e" ("c->e"))
            ("f" ("b->f" "e->f"))
            ("g" ("c->g" "e->g")) ("h" NIL)
            ("j" ("h->j")) ("k" NIL))
          :do (is-true
               (equal (mnas-graph:ids
                       (mnas-graph:inlet-edges
                        (mnas-graph:find-node g node)))
                      rez)))))

(def-test test-outlet-edges ()
  "
  (loop :for node :in (mnas-graph:ids
                       (mnas-graph:nodes *g*))
        :collect (list
                  node
                  (mnas-graph:ids
                   (mnas-graph:outlet-edges
                    (mnas-graph:find-node *g* node)))))
"
  (with-fixture fix-graph-g ()
    (loop :for (node rez) :in
          '(("a" ("a->c")) ("b" ("b->c" "b->f")) ("c" ("c->d" "c->e" "c->g")) ("d" NIL)
            ("e" ("e->f" "e->g")) ("f" NIL) ("g" NIL) ("h" ("h->j")) ("j" NIL) ("k" NIL))
          :do (is-true
               (equal (mnas-graph:ids
                       (mnas-graph:outlet-edges
                        (mnas-graph:find-node g node)))
                      rez)))))

(def-test test-isolated-p ()
  "
  (loop :for node :in (mnas-graph:ids
                       (mnas-graph:nodes *g*))
        :collect (list
                  node
                  (mnas-graph:isolated-p
                    (mnas-graph:find-node *g* node))))
"
  (with-fixture fix-graph-g ()
    (loop :for (node rez) :in
          '(("a" NIL) ("b" NIL) ("c" NIL) ("d" NIL) ("e" NIL)
            ("f" NIL) ("g" NIL) ("h" NIL) ("j" NIL) ("k" T))
          :do (is-true
               (equal (mnas-graph:isolated-p
                        (mnas-graph:find-node g node))
                      rez)))))

(def-test test-inlet-p ()
  "
  (loop :for node :in (mnas-graph:ids
                       (mnas-graph:nodes *g*))
        :collect (list
                  node
                  (mnas-graph:inlet-p
                    (mnas-graph:find-node *g* node))))
"
  (with-fixture fix-graph-g ()
    (loop :for (node rez) :in
          '(("a" NIL) ("b" NIL) ("c" NIL) ("d" T) ("e" NIL)
            ("f" T) ("g" T) ("h" NIL) ("j" T) ("k" NIL))
          :do (is-true
               (equal (mnas-graph:inlet-p
                       (mnas-graph:find-node g node))
                      rez)))))

(def-test test-outlet-p ()
  "
  (loop :for node :in (mnas-graph:ids
                       (mnas-graph:nodes *g*))
        :collect (list
                  node
                  (mnas-graph:outlet-p
                    (mnas-graph:find-node *g* node))))
"
  (with-fixture fix-graph-g ()
    (loop :for (node rez) :in
          '(("a" T) ("b" T) ("c" NIL) ("d" NIL) ("e" NIL)
            ("f" NIL) ("g" NIL) ("h" T) ("j" NIL) ("k" NIL))
          :do (is-true
               (equal (mnas-graph:outlet-p
                       (mnas-graph:find-node g node))
                      rez)))))

(def-test test-inlet-nodes ()
  "
  (mnas-graph:ids (mnas-graph::inlet-nodes *g*))

"
  (with-fixture fix-graph-g ()
    (is-true
     (equal (mnas-graph:ids
             (mnas-graph:inlet-nodes g))
            '("d" "f" "g" "j")))))

(def-test test-outlet-nodes ()
  " (mnas-graph:ids (mnas-graph::outlet-nodes *g*)) "
  (with-fixture fix-graph-g ()
    (is-true
     (equal (mnas-graph:ids
             (mnas-graph:outlet-nodes g))
            '("a" "b" "h")))))

(def-test test-outlet-nodes ()
  " (mnas-graph:ids (mnas-graph::outlet-nodes *g*)) "
  (with-fixture fix-graph-g ()
    (is-true
     (equal (mnas-graph:ids
             (mnas-graph:isolated-nodes g))
            '("k")))))




(mnas-graph/view:view-graph  :graphviz-prg :filter-neato)

#+nil
(

(defparameter *g*
    (mnas-graph:make-graph '(("a" "c") ("b" "c") ("c" "d")
                             ("c" "g") ("c" "e") ("e" "f")
                             ("e" "g") ("h" "j") ("b" "f"))
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
