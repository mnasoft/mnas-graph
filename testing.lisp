;;;; testing.lisp

(in-package #:mnas-graph)

(progn
  (defparameter *g*     (make-instance 'graph ))
  (defparameter *v1-v2* (make-instance 'rib
			    :from (make-instance 'ver :node "A")
			    :to   (make-instance 'ver :node "B")))

  (insert-to *v1-v2* *g*)
  (insert-to (make-instance 'ver :node "C") *g*)
  (insert-to (make-instance 'ver :node "D") *g*)
  )

;;(defparameter *v1-v2* (make-instance 'rib :from (graph-find-vertex-by-name *g* "A") :to   (graph-find-vertex-by-name *g* "B")))

(with-open-file (os "~/quicklisp/local-projects/mnas/mnas-graph/123.gv" :direction :output :if-exists :supersede)
  (to-graphviz *g* os))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter data
  '(("a" "b") ("a" "c") ("a" "d") ("b" "c") ("c" "d")))

(with-open-file (os "~/quicklisp/local-projects/mnas/mnas-graph/123.gv" :direction :output :if-exists :supersede)
  (to-graphviz
   (generate-graph
    '((#1="Ванцовский" #2="Коротич")
      (#1#             #3="Петельчиц")
      (#3#             "Федоров")
      (#3#             "Снигирь")
      (#3#             "Тройннич")
      (#3#             "Зинченко")
      (#2#             #4="Головерда")
      (#2#             "Рябов")
      (#2#             "Пивень")
      (#2#             "Банкулова")
      (#2#             "Матвеев")
      (#4#             "Иванов")
      (#2#             "Давлеткужин")
      (#4#             "Гришина")))
   os))

