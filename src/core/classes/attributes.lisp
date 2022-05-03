;;;;./src/core/classes/attributes.lisp

(in-package #:mnas-graph)

(defmacro class-slot (slot doc-slot doc-class)
  (let ((class    (read-from-string (string-downcase (format nil "<~A>" slot))))
        (initarg  (read-from-string (string-downcase (format nil ":~A"  slot))))
        (slot-doc (format nil "Type: ~{~A~^, ~}"
                          (mapcar #'(lambda (el)
                                      (concatenate 'string 
                                                   "@link[uri=\""
                                                   "https://graphviz.org/docs/"
                                                   el "/\"]("
                                                   (car(last (mnas-string:split "/" el)))
                                                   ")"))
                                  (mnas-string:split
                                   (coerce  (list #\Space #\NewLine #\Return) 'string) doc-slot))))
        (class-doc (format nil "Attribute: ~{~A~^, ~}"
                           (mapcar #'(lambda (el)
                                       (concatenate 'string 
                                                    "@link[uri=\""
                                                    "https://graphviz.org/docs/"
                                                    el "/\"]("
                                                    (car(last (mnas-string:split "/" el)))
                                                    ")"))
                                   (mnas-string:split
                                    (coerce  (list #\Space #\NewLine #\Return) 'string) doc-class)))))
    `(defclass ,class ()
       ((,slot
         :accessor ,slot
         :initarg  ,initarg
         :initform nil
         :documentation ,slot-doc))
       (:documentation  ,class-doc))))

(defmacro pr-obj-after (slot)
  (let ((class (read-from-string (string-downcase (format nil "<~A>" slot))))
        (fmt   (concatenate 'string
                            (string-downcase
                             (format nil "~S" slot))
                            "=~S ")))
    `(defmethod print-object :after ((,slot ,class) s)
       (when (,slot ,slot) 
         (format s ,fmt (,slot ,slot))))))

(defmacro def-my-class (slot doc-slot doc-class)
  `(list
    (class-slot   ,slot ,doc-slot ,doc-class)
    (pr-obj-after ,slot)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-my-class shape
  "attr-types/shape" "attrs/shape")

(def-my-class color
  "attr-types/color attr-types/colorList"
  "attrs/color")

(def-my-class fillcolor
  "attr-types/color attr-types/colorList"
  "attrs/fillcolor")

(def-my-class style
  "attr-types/style"
  "attrs/style")

(def-my-class label
  "attr-types/lblString"
  "attrs/label")

(def-my-class image
  "attr-types/string"
  "attrs/image")

(def-my-class labelloc
  "attr-types/string"
  "attrs/labelloc")

(def-my-class arrowhead
  "attr-types/arrowType"
  "attrs/arrowhead")

(def-my-class arrowtail
  "attr-types/arrowType"
  "attrs/arrowtail")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass <node-attributes>
    (
     <color> <shape>  <fillcolor> <style> <label> <image> <labelloc>
     )
  ()
  (:documentation "@link[uri=\"https://graphviz.org/doc/info/attrs.html\"](Attributes) "))

(defclass <edge-attributes>
    (
                                        ;<color> <arrowhead> <arrowtail>
     )
  ()
  (:documentation "@link[uri=\"https://graphviz.org/doc/info/attrs.html\"](Attributes) "))
