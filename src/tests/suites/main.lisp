;;;; tests/main.lisp

(in-package :mnas-graph/tests)

(def-suite main
  :description "Мастер-набор всех тестов проекта mnas-graph."
  :in all)

(in-suite main)

(defparameter *g*
  (mnas-graph:make-graph '(("a" "c") ("c" "d") ("c" "g") ("c" "e")
                           ("e" "f") ("e" "g") ("h" "j") ("b" "f"))
                           :nodes
                           '("k")))

(def-fixture fix-graph-g ()
  (let ((g (mnas-graph:make-graph
            '(("a" "c") ("c" "d") ("c" "g") ("c" "e")
              ("e" "f") ("e" "g") ("h" "j") ("b" "f"))
            :nodes
            '("k"))))
    (&body)))

(def-test test-into-container-p-nodes ()
  (with-fixture fix-graph-g ()
    (loop :for (node rez) :in
          #+nil (loop :for node :in (mnas-graph:ids
                                     (mnas-graph:nodes *g*))
                      :collect (list node (mnas-graph:into-container-p
                                           (mnas-graph:find-node node *g*)
                                           *g*)))
          '(("a" T) ("b" T) ("c" T) ("d" T) ("e" T) ("f" T) ("g" T) ("h" T) ("j" T)
            ("k" T))
          :do (is-true
               (equal (mnas-graph:into-container-p
                       (mnas-graph:find-node node g)
                       g)
                      rez)))))

(def-test test-into-container-p-edges ()
  (with-fixture fix-graph-g ()
    (loop :for (edge rez) :in
          #+nil (loop :for edge :in (mnas-graph:ids
                                     (mnas-graph:edges *g*))
                      :collect (list edge (mnas-graph:into-container-p
                                           (mnas-graph:find-edge edge *g*)
                                           *g*)))
          '(("a->c" T) ("b->f" T) ("c->d" T) ("c->e" T)
            ("c->g" T) ("e->f" T) ("e->g" T) ("h->j" T))
          :do (is-true
               (equal (mnas-graph:into-container-p
                       (mnas-graph:find-edge edge g)
                       g)
                      rez)))))

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
                                            (mnas-graph:find-node node *g*)
                                            *g*))))
          '(("a" NIL) ("b" NIL) ("c" ("a->c")) ("d" ("c->d")) ("e" ("c->e"))
            ("f" ("b->f" "e->f")) ("g" ("c->g" "e->g")) ("h" NIL) ("j" ("h->j")) ("k" NIL))
          :do (is-true
               (equal (mnas-graph:ids
                       (mnas-graph:inlet-edges
                        (mnas-graph:find-node node g)
                        g))
                      rez)))))

(def-test test-outlet-edges ()
  (with-fixture fix-graph-g ()
    (loop :for (node rez) :in
          #+nil (loop :for node :in (mnas-graph:ids
                                     (mnas-graph:nodes *g*))
                      :collect
                      (list node (mnas-graph:ids
                                  (mnas-graph:outlet-edges
                                   (mnas-graph:find-node node *g*)
                                   *g*))))
          '(("a" ("a->c")) ("b" ("b->f")) ("c" ("c->d" "c->e" "c->g")) ("d" NIL)
            ("e" ("e->f" "e->g")) ("f" NIL) ("g" NIL) ("h" ("h->j")) ("j" NIL) ("k" NIL))
          :do (is-true
               (equal (mnas-graph:ids
                       (mnas-graph:outlet-edges
                        (mnas-graph:find-node node g)
                        g))
                      rez)))))

(def-test test-both-edges ()
  (with-fixture fix-graph-g ()
    (loop :for (node rez) :in
          #+nil (loop :for node :in (mnas-graph:ids
                                     (mnas-graph:nodes *g*))
                      :collect
                      (list node (mnas-graph:ids
                                  (mnas-graph:both-edges
                                   (mnas-graph:find-node node *g*) *g*))))
          '(("a" ("a->c")) ("b" ("b->f")) ("c" ("a->c" "c->d" "c->e" "c->g"))
            ("d" ("c->d")) ("e" ("c->e" "e->f" "e->g")) ("f" ("b->f" "e->f"))
            ("g" ("c->g" "e->g")) ("h" ("h->j")) ("j" ("h->j")) ("k" NIL))
          :do (is-true
               (equal (mnas-graph:ids
                       (mnas-graph:both-edges
                        (mnas-graph:find-node node g) g))
                      rez)))))

(def-test test-isolated-p ()
  (with-fixture fix-graph-g ()
    (loop :for (node rez) :in
          #+nil (loop :for node :in (mnas-graph:ids
                                     (mnas-graph:nodes *g*))
                      :collect (list
                                node
                                (mnas-graph:isolated-p
                                 (mnas-graph:find-node node *g*)
                                 *g*)))
          '(("a" NIL) ("b" NIL) ("c" NIL) ("d" NIL) ("e" NIL) ("f" NIL) ("g" NIL)
            ("h" NIL) ("j" NIL) ("k" T))
          :do (is-true
               (equal (mnas-graph:isolated-p
                       (mnas-graph:find-node node g) g)
                      rez)))))

(def-test test-inlet-p ()
  (with-fixture fix-graph-g ()
    (loop :for (node rez) :in
          #+nil (loop :for node :in (mnas-graph:ids
                                     (mnas-graph:nodes *g*))
                      :collect (list node
                                     (mnas-graph:inlet-p
                                      (mnas-graph:find-node node *g*) *g*)))
          '(("a" NIL) ("b" NIL) ("c" NIL) ("d" T) ("e" NIL)
            ("f" T) ("g" T) ("h" NIL) ("j" T) ("k" NIL))
          :do (is-true
               (equal (mnas-graph:inlet-p
                       (mnas-graph:find-node node g) g)
                      rez)))))

(def-test test-outlet-p ()
  (with-fixture fix-graph-g ()
    (loop :for (node rez) :in
          #+nil(loop :for node :in (mnas-graph:ids
                                    (mnas-graph:nodes *g*))
                     :collect (list node (mnas-graph:outlet-p
                                          (mnas-graph:find-node node *g*)
                                          *g*)))
          '(("a" T) ("b" T) ("c" NIL) ("d" NIL) ("e" NIL) ("f" NIL) ("g" NIL) ("h" T)
            ("j" NIL) ("k" NIL))
          :do (is-true
               (equal (mnas-graph:outlet-p
                       (mnas-graph:find-node node g) g)
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


(def-test test-find-backward-nodes ()
  (with-fixture fix-graph-g ()
    (loop :for (node rez) :in
          #+nil (loop :for node :in (mnas-graph:ids
                                     (mnas-graph:nodes *g*))
                      :collect (list node
                                     (mnas-graph:ids
                                      (mnas-graph:find-backward-nodes
                                       (mnas-graph:find-node node *g*) *g*))))
          
          '(("a" NIL) ("b" NIL) ("c" ("a")) ("d" ("c")) ("e" ("c")) ("f" ("b" "e"))
            ("g" ("c" "e")) ("h" NIL) ("j" ("h")) ("k" NIL))
          :do (is-true
               (equal (mnas-graph:ids
                       (mnas-graph:find-backward-nodes
                        (mnas-graph:find-node node g) g))
                      rez)))))
;;;;

(def-test test-find-forward-nodes ()
  (with-fixture fix-graph-g ()
    (loop :for (node rez) :in
          #+nil (loop :for node :in (mnas-graph:ids
                                     (mnas-graph:nodes *g*))
                      :collect (list node
                                     (mnas-graph:ids
                                      (mnas-graph:find-forward-nodes
                                       (mnas-graph:find-node node *g*) *g*))))
          '(("a" ("c")) ("b" ("f")) ("c" ("d" "e" "g")) ("d" NIL) ("e" ("f" "g"))
            ("f" NIL) ("g" NIL) ("h" ("j")) ("j" NIL) ("k" NIL))
          :do (is-true
               (equal (mnas-graph:ids
                       (mnas-graph:find-forward-nodes
                        (mnas-graph:find-node node g) g))
                      rez)))))

(def-test test-find-both-nodes ()
  (with-fixture fix-graph-g ()
    (loop :for (node rez) :in
          #+nil (loop :for node :in (mnas-graph:ids
                                     (mnas-graph:nodes *g*))
                      :collect (list node
                                     (mnas-graph:ids
                                      (mnas-graph:find-both-nodes
                                       (mnas-graph:find-node node *g*)
                                       *g*))))
          '(("a" ("c")) ("b" ("f")) ("c" ("a" "d" "e" "g")) ("d" ("c"))
            ("e" ("c" "f" "g")) ("f" ("b" "e")) ("g" ("c" "e"))
            ("h" ("j")) ("j" ("h")) ("k" NIL))
          :do (is-true
               (equal (mnas-graph:ids
                       (mnas-graph:find-both-nodes
                        (mnas-graph:find-node node g)
                        g))
                      rez)))))

(def-test test-connected-nodes ()
  (with-fixture fix-graph-g ()
    (loop :for (node rez) :in
          #+nil (loop :for node :in (mnas-graph:ids
                                     (mnas-graph:nodes *g*))
                      :collect (list node
                                     (mnas-graph:ids
                                      (mnas-graph:connected-nodes
                                       (mnas-graph:find-node node *g*) *g*
                                       :direction :backward))))
          '(("a" ("a")) ("b" ("b")) ("c" ("a" "c")) ("d" ("a" "c" "d"))
            ("e" ("a" "c" "e")) ("f" ("a" "b" "c" "e" "f")) ("g" ("a" "c" "e" "g"))
            ("h" ("h")) ("j" ("h" "j")) ("k" ("k")))
          :do (is-true
               (equal (mnas-graph:ids
                       (mnas-graph:connected-nodes
                        (mnas-graph:find-node node g) g
                        :direction :backward))
                      rez)))
    (loop :for (node rez) :in
          #+nil (loop :for node :in (mnas-graph:ids
                                     (mnas-graph:nodes *g*))
                      :collect (list node
                                     (mnas-graph:ids
                                      (mnas-graph:connected-nodes
                                       (mnas-graph:find-node node *g*) *g*
                                       :direction :forward))))
          '(("a" ("a" "c" "d" "e" "f" "g")) ("b" ("b" "f")) ("c" ("c" "d" "e" "f" "g"))
            ("d" ("d")) ("e" ("e" "f" "g")) ("f" ("f")) ("g" ("g")) ("h" ("h" "j"))
            ("j" ("j")) ("k" ("k")))
          :do (is-true
               (equal (mnas-graph:ids
                       (mnas-graph:connected-nodes
                        (mnas-graph:find-node node g) g
                        :direction :forward))
                      rez)))
    (loop :for (node rez) :in
          #+nil (loop :for node :in (mnas-graph:ids
                                     (mnas-graph:nodes *g*))
                      :collect (list node
                                     (mnas-graph:ids
                                      (mnas-graph:connected-nodes
                                       (mnas-graph:find-node node *g*) *g*
                                       :direction :both))))
          '(("a" ("a" "b" "c" "d" "e" "f" "g")) ("b" ("a" "b" "c" "d" "e" "f" "g"))
            ("c" ("a" "b" "c" "d" "e" "f" "g")) ("d" ("a" "b" "c" "d" "e" "f" "g"))
            ("e" ("a" "b" "c" "d" "e" "f" "g")) ("f" ("a" "b" "c" "d" "e" "f" "g"))
            ("g" ("a" "b" "c" "d" "e" "f" "g")) ("h" ("h" "j")) ("j" ("h" "j"))
            ("k" ("k")))
          :do (is-true
               (equal
                (mnas-graph:ids
                 (mnas-graph:connected-nodes
                  (mnas-graph:find-node node g) g
                  :direction :both))
                rez)))
    
    (loop :for (depth rez) :in
          #+nil (loop :for depth :in '(1 2 3 4 5)
                      :collect
                      (list depth
                            (mnas-graph:ids
                             (mnas-graph:connected-nodes
                              (mnas-graph:find-node "a" *g*) *g*
                              :direction :both :depth depth))))
          '((1 ("a" "c")) (2 ("a" "c" "d" "e" "g")) (3 ("a" "c" "d" "e" "f" "g"))
            (4 ("a" "b" "c" "d" "e" "f" "g")) (5 ("a" "b" "c" "d" "e" "f" "g")))
          :do (is-true
               (equal
                (mnas-graph:ids
                             (mnas-graph:connected-nodes
                              (mnas-graph:find-node "a" g) g
                              :direction :both :depth depth))
                rez)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-test test-remove-from ()
  (with-fixture fix-graph-g ()
    (loop :for (node rez) :in
          #+nil
           (loop :for node :in
                           (mnas-graph:ids
                            (mnas-graph:nodes *g*))
                 :collect (list node
                                (let ((n-*g* (mnas-graph:copy *g*))) 
                                  (mnas-graph:remove-from
                                   (mnas-graph:find-node node n-*g*)
                                   n-*g*)
                                  (mnas-graph:ids n-*g*))))
          '(("a"
             ((("b" "f") ("c" "d") ("c" "e") ("c" "g") ("e" "f") ("e" "g") ("h" "j"))
              :NODES ("k")))
            ("b"
             ((("a" "c") ("c" "d") ("c" "e") ("c" "g") ("e" "f") ("e" "g") ("h" "j"))
              :NODES ("k")))
            ("c" ((("b" "f") ("e" "f") ("e" "g") ("h" "j")) :NODES ("a" "d" "k")))
            ("d"
             ((("a" "c") ("b" "f") ("c" "e") ("c" "g") ("e" "f") ("e" "g") ("h" "j"))
              :NODES ("k")))
            ("e" ((("a" "c") ("b" "f") ("c" "d") ("c" "g") ("h" "j")) :NODES ("k")))
            ("f"
             ((("a" "c") ("c" "d") ("c" "e") ("c" "g") ("e" "g") ("h" "j")) :NODES
              ("b" "k")))
            ("g"
             ((("a" "c") ("b" "f") ("c" "d") ("c" "e") ("e" "f") ("h" "j")) :NODES ("k")))
            ("h"
             ((("a" "c") ("b" "f") ("c" "d") ("c" "e") ("c" "g") ("e" "f") ("e" "g"))
              :NODES ("j" "k")))
            ("j"
             ((("a" "c") ("b" "f") ("c" "d") ("c" "e") ("c" "g") ("e" "f") ("e" "g"))
              :NODES ("h" "k")))
            ("k"
             ((("a" "c") ("b" "f") ("c" "d") ("c" "e") ("c" "g") ("e" "f") ("e" "g")
               ("h" "j"))
              :NODES NIL)))
          :do (is-true
               (equal
                (let ((n-g (mnas-graph:copy g)))
                  (mnas-graph:remove-from 
                   (mnas-graph:find-node node n-g) n-g)
                  (mnas-graph:ids n-g))
                rez)))))

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

(mnas-graph/view:view-graph *g*)
 )

(mnas-hash-table:to-list
 (mnas-graph:inlet-edges (mnas-graph:find-node "b" *g*) *g*))

(mnas-hash-table:to-list
 (mnas-graph:outlet-edges (mnas-graph:find-node "k" *g*) *g*))
