;;;; tests/main.lisp

(in-package :mnas-graph/tests)

(def-suite main
  :description "Мастер-набор всех тестов проекта mnas-graph."
  :in all)

(in-suite main)

(def-test section-variables ()
  (is-true (= 5 5))
  )

(progn 
  (defparameter *g*
    (mnas-graph:make-graph '(("a" "c") ("b" "c") ("c" "d")
                             ("c" "g") ("c" "e") ("e" "f")
                             ("e" "g") ("h" "j") ("b" "f"))
                           :nodes
                           '("k")))
  (setf (mnas-graph::color (mnas-graph:find-node  *g* "a")) "red")
  (setf (mnas-graph::shape (mnas-graph:find-node  *g* "a")) "ellipse")
  (mnas-graph::to-string (mnas-graph:find-node  *g* "a"))
  (mnas-graph::to-string (mnas-graph:find-edge  *g* "a->c"))
  (mnas-graph:name (mnas-graph:find-edge  *g* "a->c"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
  (mnas-graph/view::to-graphviz *g* t)
  (mnas-graph/view:view-graph *g*)

  (mnas-graph:name-edges *g*)
  (mnas-graph:name
   (mnas-graph:find-node  *g* "a"))
  
  (mnas-graph:to-nodes "c" *g*)
  (mnas-graph:from-nodes "c" *g*)

  (mnas-graph:nea-from-nodes
   (mnas-graph:find-node  *g* "c"))
  (mnas-graph:nea-to-nodes
   (mnas-graph:find-node  *g* "c"))
  (mnas-graph:connected-nodes
   (mnas-graph:find-node  *g* "a")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-test section-variables ()
  (is-true (= 5 5))
  )

(progn
  (defparameter *g*
    (make-instance 'mnas-graph:<graph>))
  (defparameter *e*
    (make-instance
     'mnas-graph:<edge>
     :owner *g*
     :tail (make-instance
            'mnas-graph:<node>
            :owner *g* :name "a")
     :head (make-instance
            'mnas-graph:<node>
            :owner *g* :name "b")))
  ;;#+nil
  (setf (mnas-graph::color
         (mnas-graph:find-node *g* "a"))
        "red")
  #+nil  (setf (mnas-graph::color
                (mnas-graph:find-edge *g* "a->b"))
               "green")
  #+nil
  (mnas-graph/view:view-graph *g*))

(defparameter *n-t* (make-instance
            'mnas-graph:<node> 
            ;;:owner *g*
            :name "a" :color "red"))

(mnas-graph:name   *n-t*)
(mnas-graph::color *n-t*)

(make-instance 'mnas-graph:<color>)
*n-t*

