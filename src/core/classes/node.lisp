;;;; ./src/core/classes/node.lisp

(in-package #:mnas-graph)

(defclass <node> (<node-attributes>)
  ((name
    :accessor name
    :initarg :name
    :initform nil
    :documentation "Имя вершины")
   (owner
    :accessor owner
    :initarg :owner
    :initform nil
    :documentation "Владелец вершины объект типа graph")
;;;

;;;;   
   (counter :accessor <node>-counter               :initform 0   :documentation "Количество, созданных вершин" :allocation :class))
  (:documentation "@b(Описание:) класс @b(<node>) представляет вершину графа."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod initialize-instance :around ((x <node>)
                                        &key
                                          name owner
;;;;                                          
                                          shape color fillcolor style
                                          label image labelloc)
  (call-next-method x
		    :name      name
    		    :owner     owner
;;;;
                    :shape     shape
                    :color     color
                    :fillcolor fillcolor
                    :style     style
                    :label     label
                    :image     image
                    :labelloc  labelloc
		    :number (<node>-counter x))
  (when owner (insert-to x owner))
  (incf (<node>-counter x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-object        ((x <node>) s))

#+nil(defmethod print-object :after ((x <node>) s)
       (format s "~S:~S"   (not(null (owner x))) (name x)))

(defmethod print-object :after ((node <node>) s)
  (format s "~S" (name node))
  (let ((props
          (loop :for (key val) :in
                `(("shape"     ,(shape     node))
                  ("color"     ,(color     node))
                  ("fillcolor" ,(fillcolor node))
                  ("style"     ,(style     node))
                  ("label"     ,(label     node))
                  ("image"     ,(image     node))
                  ("labelloc"  ,(labelloc  node)))
                :when val :collect (format nil "~A=~S" key val))))
    (when props (format s " [~{~A~^, ~}]~%" props))))
