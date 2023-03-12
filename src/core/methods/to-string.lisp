;;;; ./src/core/methods/to-string.lisp

(in-package :mnas-graph)

(defmethod to-string (val)
  "@b(Описание:) to-string !!!!!!
"
  (format nil "~A" val))

(defmethod to-string ((x <node>))
  "@b(Описание:) to-string !!!!!!
"
  (format nil "~A" (name x)))

(defmethod to-string ((x <edge>))
  "@b(Описание:) to-string !!!!!!
"
  (format nil "~A->~A"
           (name (tail x))
           (name (head x))))

