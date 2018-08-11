;;;; testing.lisp

(in-package #:mnas-graph)

(view-graph
 (generate-graph
  '(("a" "b") ("a" "c") ("a" "d") ("b" "c") ("c" "d"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(view-graph
 (generate-graph
  '(("a" "b") ("b" "c") ("c" "a"))) :graphviz-prg *circo-path*)

(view-graph
 (generate-graph
  '(("a" "b") ("b" "c") ("c" "d") ("d" "a"))) :graphviz-prg *circo-path*)

(view-graph
 (generate-graph
  '(("a" "b") ("b" "c") ("c" "d") ("d" "e") ("e" "a"))) :graphviz-prg *circo-path*)

(view-graph
 (generate-graph
  '(("a" "b") ("b" "c") ("c" "d") ("d" "e") ("e" "f") ("f" "a"))) :graphviz-prg *circo-path*)

(view-graph
 (generate-graph
  '(("a" "b") ("b" "c") ("c" "d") ("d" "e") ("e" "f") ("f" "g") ("g" "a"))) :graphviz-prg *circo-path*)

(view-graph
 (generate-graph
  '(("a" "b") ("b" "c") ("c" "d") ("d" "e") ("e" "f") ("f" "g") ("g" "h") ("h" "a"))) :graphviz-prg *circo-path*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(progn
  (view-graph
   (generate-graph
    '(("a" "b") ("b" "c") ("c" "a")
      ("d" "a") ("b" "d") ("c" "d") )) :graphviz-prg *sfdp-path*)

  (view-graph
   (generate-graph
    '(("a" "b") ("b" "c") ("c" "d") ("d" "a")
      ("a" "A") ("b" "B") ("c" "C") ("d" "D")
      ("A" "B") ("B" "C") ("C" "D") ("D" "A")
      )) :graphviz-prg *fdp-path*)

    (view-graph
     (generate-graph
      '(("a" "b") ("b" "c") ("c" "d") ("d" "a")
	("p" "a") ("p" "b") ("p" "c") ("p" "d")
	("a" "r") ("b" "r") ("c" "r") ("d" "r")
	)) :graphviz-prg *sfdp-path*)

  (view-graph
   (generate-graph
    '(("a" "b") ("b" "c") ("c" "d") ("d" "e") ("e" "a")
      ("a" "a1") ("b" "b1") ("c" "c1") ("d" "d1") ("e" "e1")

      ("A1" "a1") ("B1" "b1") ("C1" "c1") ("D1" "d1") ("E1" "e1")
      ("A1" "b1") ("B1" "c1") ("C1" "d1") ("D1" "e1") ("E1" "a1")
    
      ("A" "A1") ("B" "B1") ("C" "C1") ("D" "D1") ("E" "E1")
      ("A" "B") ("B" "C") ("C" "D") ("D" "E") ("E" "A")
      )) :graphviz-prg *fdp-path*))

(view-graph
 (generate-graph
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
    (#4#             "Гришина"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun test-3 (&key (node-max-number 100) (edges-number node-max-number) (graphviz-prg *sfdp-path*))
  (view-graph
   (generate-graph
    (let ((lst nil))
      (dotimes (i edges-number lst)
	(push (list
	       (format nil "~A" (random node-max-number))
	       (format nil "~A" (random node-max-number)))
	      lst))))
   :graphviz-prg graphviz-prg ))

(progn (test-3 :node-max-number 10    :edges-number 6)
       (test-3 :node-max-number 16    :edges-number 10)
       (test-3 :node-max-number 25    :edges-number 16)
       (test-3 :node-max-number 40    :edges-number 25)
       (test-3 :node-max-number 63    :edges-number 40)
       (test-3 :node-max-number 100   :edges-number 63)
       (test-3 :node-max-number 160   :edges-number 100)
       (test-3 :node-max-number 250   :edges-number 160)
       (test-3 :node-max-number 400   :edges-number 250)
       (test-3 :node-max-number 630   :edges-number 400)
       (test-3 :node-max-number 1000  :edges-number 630))
