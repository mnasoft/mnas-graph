;;;; testing.lisp

(in-package #:mnas-graph)

(progn
  (defparameter *g*     (make-instance 'graph ))
  (defparameter *v1-v2* (make-instance 'edge
			    :from (make-instance 'node :node "A")
			    :to   (make-instance 'node :node "B")))

  (insert-to *v1-v2* *g*)
  (insert-to (make-instance 'node :node "C") *g*)
  (insert-to (make-instance 'node :node "D") *g*)
  )

;;(defparameter *v1-v2* (make-instance 'rib :from (graph-find-vertex-by-name *g* "A") :to   (graph-find-vertex-by-name *g* "B")))

(with-open-file (os "~/123.gv" :direction :output :if-exists :supersede)
  (to-graphviz *g* os))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter data
  '(("a" "b") ("a" "c") ("a" "d") ("b" "c") ("c" "d")))

(defparameter data-1
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
      (#4#             "Гришина"))))

(with-open-file (os "~/quicklisp/local-projects/mnas/mnas-graph/123.gv" :direction :output :if-exists :supersede)
  (to-graphviz data-1 os))

(view-graph data-1 "cool")

(with-open-file (out (concatenate 'string fpath "/" fname ".gv")
		       :direction :output :if-exists :supersede :external-format :UTF8)
    (to-graphviz g out))
