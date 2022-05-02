;;;;./src/core/classes/attributes.lisp

(in-package #:mnas-graph)

(defclass <shape> ()
  ((shape
    :accessor shape
    :initarg :shape
    :initform nil
    :documentation "shape - box, ellipse, ...

@link[uri=\"https://graphviz.org/docs/attrs/shape/\"](shape) "))
  (:documentation "@link[uri=\"https://graphviz.org/docs/attrs/shape/\"](shape)"))

(defmethod print-object :after ((x <shape>) s)
  (format s "shape=~S " (shape x)))

(defclass <color> ()
  ((color
    :accessor color
    :initarg :color
    :initform nil
    :documentation "color - red, blue, ...
@link[uri=\"https://graphviz.org/docs/attrs/color/\"](color)
")))

(defclass <fillcolor> ()
  ((fillcolor
    :accessor fillcolor
    :initarg :fillcolor
    :initform nil
    :documentation "fillcolor - red, yellow, ...
@link[uri=\"https://graphviz.org/docs/attrs/fillcolor/\"](fillcolor)
")))

(defclass <style> ()
  ((style
    :accessor style
    :initarg :style
    :initform nil
    :documentation "style - bold, ... "))
   (:documentation "@link[uri=\"https://graphviz.org/docs/attrs/style/\"](style)"))

(defclass <label> ()
  ((label
    :accessor label
    :initarg :label
    :initform nil
    :documentation "label - \"John Fitzgerald Kennedy\nb. 29.5.1917 Brookline\nd. 22.11.1963 Dallas\"

@link[uri=\"https://graphviz.org/docs/attrs/label/\"](label)"))
   (:documentation "@link[uri=\"https://graphviz.org/docs/attrs/label/\"](label)"))

(defclass <image> ()
  ((image
    :accessor image
    :initarg :image
    :initform nil
    :documentation "image - \"images/kennedyface.jpg\""))
   (:documentation "@link[uri=\"https://graphviz.org/docs/attrs/image/\"](image)"))

(defclass <labelloc> ()
  ((labelloc
    :accessor labelloc
    :initarg :labelloc
    :initform nil
    :documentation "@link[uri=\"https://graphviz.org/docs/attrs/labelloc/\"](labelloc)"))
  (:documentation "@link[uri=\"https://graphviz.org/docs/attrs/labelloc/\"](labelloc)"))

(defclass <arrowhead> ()
  ((arrowhead
    :accessor arrowhead
    :initarg :arrowhead
    :initform nil
    :documentation "@link[uri=\"https://graphviz.org/docs/attr-types/arrowType/\"](arrowhead)"))
  (:documentation "@link[uri=\"https://graphviz.org/docs/attr-types/arrowType/\"](arrowhead)"))

(defclass <arrowtail> ()
  ((arrowtail
    :accessor arrowtail
    :initarg :arrowtail
    :initform nil
    :documentation "@link[uri=\"https://graphviz.org/docs/attr-types/arrowType/\"](arrowhead)"))
  (:documentation "@link[uri=\"https://graphviz.org/docs/attr-types/arrowType/\"](arrowhead)"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass <node-attributes>
    (<shape>
     <color>
     <fillcolor>
     <style>
     <label>
     <image>
     <labelloc>)
  ()
  (:documentation "@link[uri=\"https://graphviz.org/doc/info/attrs.html\"](Attributes) "))

(defclass <edge-attributes>
    (<color>
     <arrowhead>
     <arrowtail>)
  ()
  (:documentation "@link[uri=\"https://graphviz.org/doc/info/attrs.html\"](Attributes) "))
