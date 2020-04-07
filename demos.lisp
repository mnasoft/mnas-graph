;;;; demos.lisp

(in-package #:mnas-graph)
(annot:enable-annot-syntax)

@export
(defun demo-1 () 
  (view-graph
   (make-graph
    '(("a" "b") ("a" "c") ("a" "d") ("b" "c") ("c" "d")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@export
(defun demo-2 () 
  (view-graph
   (make-graph
    '(("a" "b") ("b" "c") ("c" "a"))) :graphviz-prg :filter-circo)
  (view-graph
   (make-graph
    '(("a" "b") ("b" "c") ("c" "d") ("d" "a"))) :graphviz-prg :filter-circo)
  (view-graph
   (make-graph
    '(("a" "b") ("b" "c") ("c" "d") ("d" "e") ("e" "a"))) :graphviz-prg :filter-circo)
  (view-graph
   (make-graph
    '(("a" "b") ("b" "c") ("c" "d") ("d" "e") ("e" "f") ("f" "a"))) :graphviz-prg :filter-circo)
  (view-graph
   (make-graph
    '(("a" "b") ("b" "c") ("c" "d") ("d" "e") ("e" "f") ("f" "g") ("g" "a"))) :graphviz-prg :filter-circo)
  (view-graph
   (make-graph
    '(("a" "b") ("b" "c") ("c" "d") ("d" "e") ("e" "f") ("f" "g") ("g" "h") ("h" "a"))) :graphviz-prg :filter-circo))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@export
(defun demo-3 () 
  (view-graph
   (make-graph
    '(("a" "b") ("b" "c") ("c" "a")
      ("d" "a") ("b" "d") ("c" "d") )) :graphviz-prg :filter-fdp)
  (view-graph
   (make-graph
    '(("a" "b") ("b" "c") ("c" "d") ("d" "a")
      ("a" "A") ("b" "B") ("c" "C") ("d" "D")
      ("A" "B") ("B" "C") ("C" "D") ("D" "A")
      )) :graphviz-prg :filter-neato)
  (view-graph
   (make-graph
    '(("a" "b") ("b" "c") ("c" "d") ("d" "a")
      ("p" "a") ("p" "b") ("p" "c") ("p" "d")
      ("a" "r") ("b" "r") ("c" "r") ("d" "r")
      )) :graphviz-prg :filter-fdp)
  (view-graph
   (make-graph
    '(("a" "b") ("b" "c") ("c" "d") ("d" "e") ("e" "a")
      ("a" "a1") ("b" "b1") ("c" "c1") ("d" "d1") ("e" "e1")

      ("A1" "a1") ("B1" "b1") ("C1" "c1") ("D1" "d1") ("E1" "e1")
      ("A1" "b1") ("B1" "c1") ("C1" "d1") ("D1" "e1") ("E1" "a1")
    
      ("A" "A1") ("B" "B1") ("C" "C1") ("D" "D1") ("E" "E1")
      ("A" "B") ("B" "C") ("C" "D") ("D" "E") ("E" "A")
      )) :graphviz-prg :filter-fdp))

@export
(defun demo-4 () 
  (view-graph
   (make-graph
    '((#1="Ванцовский" #2="Коротич")
      (#2#             #4="Головерда")
      (#1#             #3="Петельчиц")
      (#3#             "Федоров")
      (#3#             "Снигирь")
      (#3#             "Тройннич")
      (#3#             "Зинченко")
    
      (#2#             "Рябов")
      (#2#             "Пивень")
      (#2#             "Банкулова")
      (#2#             "Матвеев")
      (#4#             "Иванов")
      (#2#             "Давлеткужин")
      (#4#             "Гришина")))
   :graphviz-prg :filter-dot))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@export
(defun demo-5 ()
  (view-graph (make-random-graph :node-max-number   10 :edges-number   6) :graphviz-prg :filter-neato)
  (view-graph (make-random-graph :node-max-number   16 :edges-number  10) :graphviz-prg :filter-neato)
  (view-graph (make-random-graph :node-max-number   25 :edges-number  16) :graphviz-prg :filter-neato)
  (view-graph (make-random-graph :node-max-number   40 :edges-number  25) :graphviz-prg :filter-neato)
  (view-graph (make-random-graph :node-max-number   63 :edges-number  40) :graphviz-prg :filter-neato)
  (view-graph (make-random-graph :node-max-number  100 :edges-number  63) :graphviz-prg :filter-neato)
  (view-graph (make-random-graph :node-max-number  160 :edges-number 100) :graphviz-prg :filter-neato)
  (view-graph (make-random-graph :node-max-number  250 :edges-number 160) :graphviz-prg :filter-neato)
  (view-graph (make-random-graph :node-max-number  400 :edges-number 250) :graphviz-prg :filter-neato)
  (view-graph (make-random-graph :node-max-number  630 :edges-number 400) :graphviz-prg :filter-neato)
  (view-graph (make-random-graph :node-max-number 1000 :edges-number 630) :graphviz-prg :filter-neato))

(format t "Примеры иcпользования:
(mnas-graph:demo-1)
(mnas-graph:demo-2)
(mnas-graph:demo-3)
(mnas-graph:demo-4)
(mnas-graph:demo-5)
")
