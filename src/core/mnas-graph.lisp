;;;; .src/core/mnas-graph.lisp

(defpackage #:mnas-graph
  (:use #:cl)
  (:export attr-area        <area>
           attr-arrowhead   <arrowhead>
           attr-arrowsize   <arrowsize>
           attr-arrowtail   <arrowtail>
           attr-bb          <bb>
           attr-bgcolor     <bgcolor>
           attr-center      <center>
           attr-charset     <charset>
           attr-class       <class>
           attr-clusterrank <clusterrank>
           attr-color       <color>
           attr-colorscheme <colorscheme>
           attr-comment     <comment>
           attr-compound    <compound>
           attr-concentrate <concentrate>
           attr-constraint  <constraint>
           attr-decorate    <decorate>
           attr-defaultdist <defaultdist>
           attr-dim         <dim>
           attr-dimen       <dimen>
           attr-dir         <dir>
           attr-diredgeconstraints <diredgeconstraints>
           attr-distortion  <distortion>
           attr-dpi         <dpi>
           attr-edgehref    <edgehref>
           attr-edgetarget  <edgetarget>
           attr-edgetooltip <edgetooltip>
           attr-epsilon     <epsilon>
           attr-esep        <esep>
           attr-fillcolor   <fillcolor>
           attr-fixedsize   <fixedsize>
           attr-fontcolor   <fontcolor>
           attr-fontname    <fontname>
           attr-fontnames   <fontnames>
           attr-fontpath    <fontpath>
           attr-fontsize    <fontsize>
           attr-forcelabels <forcelabels>
           attr-gradientangle <gradientangle>
           attr-group       <group>
           attr-head_lp     <head_lp>
           attr-headclip    <headclip>
           attr-headhref    <headhref>
           attr-headlabel   <headlabel>
           attr-headport    <headport>
           attr-headtarget  <headtarget>
           attr-headtooltip <headtooltip>
           attr-height      <height>
           attr-href        <href>
           attr-id          <id>
           attr-image       <image>
           attr-imagepath   <imagepath>
           attr-imagepos    <imagepos>
           attr-imagescale  <imagescale>
           attr-inputscale  <inputscale>
           attr-label       <label>
           attr-label_scheme <label_scheme>
           attr-labelangle <labelangle>
           attr-labeldistance <labeldistance>
           attr-labelfloat <labelfloat>
           attr-labelfontcolor <labelfontcolor>
           attr-labelfontname <labelfontname>
           attr-labelfontsize <labelfontsize>
           attr-labelhref <labelhref>
           attr-labeljust <labeljust>
           attr-labelloc <labelloc>
           attr-labeltarget <labeltarget>
           attr-labeltooltip <labeltooltip>
           attr-landscape <landscape>
           attr-layer <layer>
           attr-layerlistsep <layerlistsep>
           attr-layers <layers>
           attr-layerselect <layerselect>
           attr-layersep <layersep>
           attr-layout <layout>
           attr-len <len>
           attr-levels <levels>
           attr-levelsgap <levelsgap>
           attr-lhead <lhead>
           attr-lheight <lheight>
           attr-lp <lp>
           attr-ltail <ltail>
           attr-lwidth <lwidth>
           attr-margin <margin>
           attr-maxiter <maxiter>
           attr-mclimit <mclimit>
           attr-mindist <mindist>
           attr-minlen <minlen>
           attr-mode <mode>
           attr-model <model>
           attr-mosek <mosek>
           attr-newrank <newrank>
           attr-nodesep <nodesep>
           attr-nojustify <nojustify>
           attr-normalize <normalize>
           attr-notranslate <notranslate>
           attr-nslimit <nslimit>
           attr-nslimit1 <nslimit1>
           attr-ordering <ordering>
           attr-orientation <orientation>
           attr-outputorder <outputorder>
           attr-overlap <overlap>
           attr-overlap_scaling <overlap_scaling>
           attr-overlap_shrink <overlap_shrink>
           attr-pack <pack>
           attr-packmode <packmode>
           attr-pad <pad>
           attr-page <page>
           attr-pagedir <pagedir>
           attr-pencolor <pencolor>
           attr-penwidth <penwidth>
           attr-peripheries <peripheries>
           attr-pin <pin>
           attr-pos <pos>
           attr-quadtree <quadtree>
           attr-quantum <quantum>
           attr-rank <rank>
           attr-rankdir <rankdir>
           attr-ranksep <ranksep>
           attr-ratio <ratio>
           attr-rects <rects>
           attr-regular <regular>
           attr-remincross <remincross>
           attr-repulsiveforce <repulsiveforce>
           attr-resolution <resolution>
           attr-root <root>
           attr-rotate <rotate>
           attr-rotation <rotation>
           attr-samehead <samehead>
           attr-sametail <sametail>
           attr-samplepoints <samplepoints>
           attr-scale <scale>
           attr-searchsize <searchsize>
           attr-sep <sep>
           attr-shape <shape>
           attr-shapefile <shapefile>
           attr-showboxes <showboxes>
           attr-sides <sides>
           attr-size <size>
           attr-skew <skew>
           attr-smoothing <smoothing>
           attr-sortv <sortv>
           attr-splines <splines>
           attr-start <start>
           attr-style <style>
           attr-stylesheet <stylesheet>
           attr-tail_lp <tail_lp>
           attr-tailclip <tailclip>
           attr-tailhref <tailhref>
           attr-taillabel <taillabel>
           attr-tailport <tailport>
           attr-tailtarget <tailtarget>
           attr-tailtooltip <tailtooltip>
           attr-target <target>
           attr-tooltip <tooltip>
           attr-truecolor <truecolor>
           attr-vertices <vertices>
           attr-viewport <viewport>
           attr-voro_margin <voro_margin>
           attr-weight <weight>
           attr-width <width>
           attr-xdotversion <xdotversion>
           attr-xlabel <xlabel>
           attr-xlp <xlp>
           attr-z <z>
           )
  (:export <node> 
           <edge>
           <graph>)
  (:export name
           owner          ;; Возможно подлежит удалению
           <node>-counter ;; Возможно подлежит удалению
           )
  (:export tail head weight)
  (:export nodes
           edges)
  ;; <node>
  (:export isolated-p
           inlet-p
           outlet-p
           )
  (:export find-backward-nodes
           find-forward-nodes
           find-both-nodes
           )
  (:export 
           inlet-edges
           outlet-edges
           both-edges)
  (:export connected-nodes
           )
  ;; <node> & <edge>
  (:export to-string
           )
  ;; <graph>
  (:export clear
           )
  (:export isolated-nodes
           inlet-nodes
           outlet-nodes
           )
  (:export insert-to
           remove-from
           )
  (:export find-node
           find-edge
           )
  (:export count-nodes
           count-edges
           )
  (:export make-graph
           make-random-graph
           )
  (:export ids
           edge-names
           node-names
           )
  (:export copy
           to-list
           )
  (:export into-container-p)
  (:documentation
   " Пакет @b(mnas-graph) определяет базовые функции для создания
 структуры данных типа
 @link[uri=\"https://en.wikipedia.org/wiki/Graph_(discrete_mathematics)\"](Graph)
 и ее отображения через @link[uri=\"https://graphviz.org/\"](graphviz).

 Пакет определяет следующие основные классы: 
@begin(list)
@item(@ref[id=class-node](<node>) - вершина графа;)
@item(@ref[id=class-edge](<edge>) - ребро графа;)
@item(@ref[id=class-graph](<graph>) - граф.)  
@end(list)

 @b(Пример использования:)
@begin[lang=lisp](code)
  (let*
      ((g  (make-instance 'mnas-graph:<graph>))
       (v1 (make-instance 'mnas-graph:<node> :owner g :name \"v1\"))
       (v2 (make-instance 'mnas-graph:<node> :owner g :name \"v2\"))
       (v3 (make-instance 'mnas-graph:<node> :owner g :name \"v3\"))
       (r1 (make-instance 'mnas-graph:<edge> :tail v1 :head v2))
       (r2 (make-instance 'mnas-graph:<edge> :tail v2 :head v3))
       (r3 (make-instance 'mnas-graph:<edge> :tail v3 :head v1)))
    (mnas-graph:insert-to v1 g)
    (mnas-graph:insert-to v2 g)
    (mnas-graph:insert-to v3 g)
    (mnas-graph:insert-to r1 g)
    (mnas-graph:insert-to r2 g)
    (mnas-graph:insert-to r3 g)
    (mnas-graph/view:view-graph g))
@end(code)"))

;;;; (declaim (optimize (compilation-speed 0) (debug 3) (safety 0) (space 0) (speed 0)))
;;;; (declaim (optimize (space 0) (compilation-speed 0)  (speed 0) (safety 3) (debug 3)))
;;;; (setf sb-impl::*default-external-format* :utf8)

(in-package #:mnas-graph)

(defparameter *graphviz.org* "https://graphviz.org/docs/")

;;;; make-graph data ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-graph (edges &key nodes)
  "@b(Описание:) функция @b(make-graph) возвращает граф с ребрами
 @b(edges) и вершинами вершинами @b(nodes).
 
 @b(Пример использования:)
@begin[lang=lisp](code)
  (mnas-graph/view:view-graph
   (make-graph '((\"a\" \"c\") (\"b\" \"c\") (\"c\" \"d\")
                 (\"c\" \"g\") (\"c\" \"e\") (\"e\" \"f\")
                 (\"e\" \"g\") (\"h\" \"j\") (\"b\" \"f\"))
               :nodes
               '(\"k\")))
@end(code)
"
  (let ((graph (make-instance '<graph>))
	(vs (remove-duplicates
             (apply #'append
                    (apply #'append 
                           (mapcar
                            #'(lambda (ed)
                                (list (first ed)
                                      (second ed)))
                            edges))
                    (list nodes))
             :test #'equal)))
    (mapc #'(lambda (v) (insert-to (make-instance '<node> :name v) graph)) vs)
    (mapc #'(lambda (el)
              (let ((n-tail (find-node  (first   el) graph))
                    (n-head (find-node  (second  el) graph))
                    (weight (if (null (third  el)) 1 (third el)))
                    (label  (if (null (third  el)) "" (third el))))
	        (insert-to
	         (make-instance '<edge>
			        :tail   n-tail
			        :head   n-head
                                :weight weight
                                :label  label)
	         graph)))
	  edges)
    graph))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-random-graph (&key (node-max-number 100) (edges-number node-max-number))
  "@b(Описание:) функция @b(make-random-graph) возвращает случайный граф
 с количеством ребер равным @b(edges-number) и количеством вершин не
 превышающим @b(node-max-number).

@b(Пример использования:)
@begin[lang=lisp](code)
  (mnas-graph/view:view-graph
   (make-random-graph :node-max-number 20 :edges-number 10))
@end(code)
"
  (make-graph
   (let ((lst nil))
     (dotimes (i edges-number lst)
       (push (list
	      (format nil "~A" (random node-max-number))
	      (format nil "~A" (random node-max-number)))
	     lst)))
   :nodes (loop :for i :from 0 :to node-max-number
                :collect (format nil "~A" (random node-max-number)))
   
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
