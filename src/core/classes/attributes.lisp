;;;;./src/core/classes/attributes.lisp

(in-package #:mnas-graph)

(defparameter *graphviz.org* "https://graphviz.org/docs/")

(defparameter *attributes*
  '(("area" "NC" "double")
    ("arrowhead" "E" "arrowType")
    ("arrowsize" "E" "double")
    ("arrowtail" "E" "arrowType")
    ("bb" "G" "rect")
    ("bgcolor" "GC" "color colorList")
    ("center" "G" "bool")
    ("charset" "G" "string")
    ("class" "ENCG" "string")
    ("clusterrank" "G" "clusterMode")
    ("color" "ENC" "color colorList")
    ("colorscheme" "ENCG" "string")
    ("comment" "ENG" "string")
    ("compound" "G" "bool")
    ("concentrate" "G" "bool")
    ("constraint" "E" "bool")
    ("decorate" "E" "bool")
    ("defaultdist" "G" "double")
    ("dim" "G" "int")
    ("dimen" "G" "int")
    ("dir" "E" "dirType")
    ("diredgeconstraints" "G" "string bool")
    ("distortion" "N" "double")
    ("dpi" "G" "double")
    ("edgehref" "E" "escString")
    ("edgetarget" "E" "escString")
    ("edgetooltip" "E" "escString")
    ("epsilon" "G" "double")
    ("esep" "G" "addDouble addPoint")
    ("fillcolor" "NEC" "color colorList")
    ("fixedsize" "N" "bool string")
    ("fontcolor" "ENGC" "color")
    ("fontname" "ENGC" "string")
    ("fontnames" "G" "string")
    ("fontpath" "G" "string")
    ("fontsize" "ENGC" "double")
    ("forcelabels" "G" "bool")
    ("gradientangle" "NCG" "int")
    ("group" "N" "string")
    ("head_lp" "E" "point")
    ("headclip" "E" "bool")
    ("headhref" "E" "escString")
    ("headlabel" "E" "lblString")
    ("headport" "E" "portPos")
    ("headtarget" "E" "escString")
    ("headtooltip" "E" "escString")
    ("height" "N" "double")
    ("href" "GCNE" "escString")
    ("id" "GCNE" "escString")
    ("image" "N" "string")
    ("imagepath" "G" "string")
    ("imagepos" "N" "string")
    ("imagescale" "N" "bool string")
    ("inputscale" "G" "double")
    ("label" "ENGC" "lblString")
    ("label_scheme" "G" "int")
    ("labelangle" "E" "double")
    ("labeldistance" "E" "double")
    ("labelfloat" "E" "bool")
    ("labelfontcolor" "E" "color")
    ("labelfontname" "E" "string")
    ("labelfontsize" "E" "double")
    ("labelhref" "E" "escString")
    ("labeljust" "GC" "string")
    ("labelloc" "NGC" "string")
    ("labeltarget" "E" "escString")
    ("labeltooltip" "E" "escString")
    ("landscape" "G" "bool")
    ("layer" "ENC" "layerRange")
    ("layerlistsep" "G" "string")
    ("layers" "G" "layerList")
    ("layerselect" "G" "layerRange")
    ("layersep" "G" "string")
    ("layout" "G" "string")
    ("len" "E" "double")
    ("levels" "G" "int")
    ("levelsgap" "G" "double")
    ("lhead" "E" "string")
    ("lheight" "GC" "double")
    ("lp" "EGC" "point")
    ("ltail" "E" "string")
    ("lwidth" "GC" "double")
    ("margin" "NCG" "double point")
    ("maxiter" "G" "int")
    ("mclimit" "G" "double")
    ("mindist" "G" "double")
    ("minlen" "E" "int")
    ("mode" "G" "string")
    ("model" "G" "string")
    ("mosek" "G" "bool")
    ("newrank" "G" "bool")
    ("nodesep" "G" "double")
    ("nojustify" "GCNE" "bool")
    ("normalize" "G" "double bool")
    ("notranslate" "G" "bool")
    ("nslimit" "G" "double")
    ("nslimit1" "G" "double")
    ("ordering" "GN" "string")
    ("orientation" "NG" "double  string")
    ("outputorder" "G" "outputMode")
    ("overlap" "G" "string bool")
    ("overlap_scaling" "G" "double")
    ("overlap_shrink" "G" "bool")
    ("pack" "G" "bool int false")
    ("packmode" "G" "packMode")
    ("pad" "G" "double  point")
    ("page" "G" "double  point")
    ("pagedir" "G" "pagedir")
    ("pencolor" "C" "color")
    ("penwidth" "CNE" "double")
    ("peripheries" "NC" "int")
    ("pin" "N" "bool")
    ("pos" "EN" "point splineType")
    ("quadtree" "G" "quadType bool")
    ("quantum" "G" "double")
    ("rank" "S" "rankType")
    ("rankdir" "G" "rankdir")
    ("ranksep" "G" "double doubleList")
    ("ratio" "G" "double string")
    ("rects" "N" "rect")
    ("regular" "N" "bool")
    ("remincross" "G" "bool")
    ("repulsiveforce" "G" "double")
    ("resolution" "G" "double")
    ("root" "GN" "string  bool")
    ("rotate" "G" "int")
    ("rotation" "G" "double")
    ("samehead" "E" "string")
    ("sametail" "E" "string")
    ("samplepoints" "N" "int")
    ("scale" "G" "double point")
    ("searchsize" "G" "int")
    ("sep" "G" "addDouble addPoint")
    ("shape" "N" "shape")
    ("shapefile" "N" "string")
    ("showboxes" "ENG" "int")
    ("sides" "N" "int")
    ("size" "G" "double point")
    ("skew" "N" "double")
    ("smoothing" "G" "smoothType")
    ("sortv" "GCN" "int")
    ("splines" "G" "bool string")
    ("start" "G" "startType")
    ("style" "ENCG" "style")
    ("stylesheet" "G" "string")
    ("tail_lp" "E" "point")
    ("tailclip" "E" "bool")
    ("tailhref" "E" "escString")
    ("taillabel" "E" "lblString")
    ("tailport" "E" "portPos")
    ("tailtarget" "E" "escString")
    ("tailtooltip" "E" "escString")
    ("target" "ENGC" "escString string")
    ("tooltip" "NECG" "escString")
    ("truecolor" "G" "bool")
    ("vertices" "N" "pointList")
    ("viewport" "G" "viewPort")
    ("voro_margin" "G" "double")
    ("weight" "E" "int double")
    ("width" "N" "double")
    ("xdotversion" "G" "string")
    ("xlabel" "EN" "lblString")
    ("xlp" "NE" "point")
    ("z" "N" "double"))
  "@b(Описание:) параметр @b(*attributes*) содержит таблицу
   атрибутов (см. attributes-data.org и
   @link[uri=\"https://graphviz.org/doc/info/attrs.html\"](Attributes)).")

(defun mk-attributes (attr)
  "
 @b(Пример использования:)
@begin[lang=lisp](code)
 (mk-attributes *attributes*)
@end(code)
"
    (format t "~{~A~%~}"
     (loop :for (name used-by  type) :in attr
           :collect
           (format nil "(def-my-class ~A \"~{~A~^ ~}\" ~S)"
                   name
                   (mapcar
                    #'(lambda (el)
                        (concatenate 'string "attr-types" "/" el))
                    (mnas-string:split " " type))
                   (concatenate 'string "attrs" "/" name)))))

(defun mk-collected (attr cl-name find-char class-doc)
  "
 @b(Пример использования:)
@begin[lang=lisp](code)
 (mk-collected *attributes* \"<node-attributes>\" #\N \"@link[uri=\"https://graphviz.org/doc/info/attrs.html\"](Attributes)\")
 (mk-collected *attributes* \"<edge-attributes>\" #\E \"@link[uri=\"https://graphviz.org/doc/info/attrs.html\"](Attributes)\")
 (mk-collected *attributes* \"<graph-attributes>\" #\G \"@link[uri=\"https://graphviz.org/doc/info/attrs.html\"](Attributes)\")
 (mk-collected *attributes* \"<subgraphs-attributes>\" #\S \"@link[uri=\"https://graphviz.org/doc/info/attrs.html\"](Attributes)\")
 (mk-collected *attributes* \"<cluster-attributes>\" #\C \"@link[uri=\"https://graphviz.org/doc/info/attrs.html\"](Attributes)\")
@end(code)
"
  (format t "(defclass ~A (~%~{~A~%~})~%()~%(:documentation ~S))~%"
          cl-name
          (loop :for (name used-by  type) :in attr
                :when (find find-char used-by)
                  :collect (concatenate 'string "<" name ">" ))
          class-doc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro class-slot (slot doc-slot doc-class)
  (let ((class-nm    (read-from-string (format nil "<~A>" slot)))
        (slot-nm     (read-from-string (format nil "sl-~A" slot)))
        (slot-acs    (read-from-string (format nil "attr-~A" slot)))
        (initarg     (read-from-string (format nil ":~A"  slot)))
        (slot-doc (format nil "Type: ~{~A~^, ~}"
                          (mapcar #'(lambda (el)
                                      (concatenate 'string 
                                                   "@link[uri=\""
                                                   *graphviz.org*
                                                   el "/\"]("
                                                   (car(last (mnas-string:split "/" el)))
                                                   ")"))
                                  (mnas-string:split
                                   (coerce  (list #\Space #\NewLine #\Return) 'string) doc-slot))))
        (class-doc (format nil "Attribute: ~{~A~^, ~}"
                           (mapcar #'(lambda (el)
                                       (concatenate 'string 
                                                    "@link[uri=\""
                                                    *graphviz.org*
                                                    el "/\"]("
                                                    (car(last (mnas-string:split "/" el)))
                                                    ")"))
                                   (mnas-string:split
                                    (coerce  (list #\Space #\NewLine #\Return) 'string) doc-class)))))
    `(defclass ,class-nm ()
       ((,slot-nm
         :accessor ,slot-acs
         :initarg  ,initarg
         :initform nil
         :documentation ,slot-doc))
       (:documentation  ,class-doc))))

(defmacro pr-obj-after (slot)
  (let ((class-nm (read-from-string (format nil "<~A>" slot)))
        (fmt      (concatenate 'string
                               (string-downcase
                                (format nil "~S" slot))
                               "=~S "))
        (slot-acs (read-from-string (format nil "attr-~A" slot)))
        (slot-arg slot))
    `(defmethod print-object :after ((,slot ,class-nm) s)
       (when (,slot-acs ,slot-arg) 
         (format s ,fmt (,slot-acs ,slot-arg))))))

(defmacro def-my-class (slot doc-slot doc-class)
  `(list
    (class-slot   ,slot ,doc-slot ,doc-class)
    (pr-obj-after ,slot)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-my-class area "attr-types/double" "attrs/area")
(def-my-class arrowhead "attr-types/arrowType" "attrs/arrowhead")
(def-my-class arrowsize "attr-types/double" "attrs/arrowsize")
(def-my-class arrowtail "attr-types/arrowType" "attrs/arrowtail")
(def-my-class bb "attr-types/rect" "attrs/bb")
(def-my-class bgcolor "attr-types/color attr-types/colorList" "attrs/bgcolor")
(def-my-class center "attr-types/bool" "attrs/center")
(def-my-class charset "attr-types/string" "attrs/charset")
(def-my-class class "attr-types/string" "attrs/class")
(def-my-class clusterrank "attr-types/clusterMode" "attrs/clusterrank")
(def-my-class color "attr-types/color attr-types/colorList" "attrs/color")
(def-my-class colorscheme "attr-types/string" "attrs/colorscheme")
(def-my-class comment "attr-types/string" "attrs/comment")
(def-my-class compound "attr-types/bool" "attrs/compound")
(def-my-class concentrate "attr-types/bool" "attrs/concentrate")
(def-my-class constraint "attr-types/bool" "attrs/constraint")
(def-my-class decorate "attr-types/bool" "attrs/decorate")
(def-my-class defaultdist "attr-types/double" "attrs/defaultdist")
(def-my-class dim "attr-types/int" "attrs/dim")
(def-my-class dimen "attr-types/int" "attrs/dimen")
(def-my-class dir "attr-types/dirType" "attrs/dir")
(def-my-class diredgeconstraints "attr-types/string attr-types/bool" "attrs/diredgeconstraints")
(def-my-class distortion "attr-types/double" "attrs/distortion")
(def-my-class dpi "attr-types/double" "attrs/dpi")
(def-my-class edgehref "attr-types/escString" "attrs/edgehref")
(def-my-class edgetarget "attr-types/escString" "attrs/edgetarget")
(def-my-class edgetooltip "attr-types/escString" "attrs/edgetooltip")
(def-my-class epsilon "attr-types/double" "attrs/epsilon")
(def-my-class esep "attr-types/addDouble attr-types/addPoint" "attrs/esep")
(def-my-class fillcolor "attr-types/color attr-types/colorList" "attrs/fillcolor")
(def-my-class fixedsize "attr-types/bool attr-types/string" "attrs/fixedsize")
(def-my-class fontcolor "attr-types/color" "attrs/fontcolor")
(def-my-class fontname "attr-types/string" "attrs/fontname")
(def-my-class fontnames "attr-types/string" "attrs/fontnames")
(def-my-class fontpath "attr-types/string" "attrs/fontpath")
(def-my-class fontsize "attr-types/double" "attrs/fontsize")
(def-my-class forcelabels "attr-types/bool" "attrs/forcelabels")
(def-my-class gradientangle "attr-types/int" "attrs/gradientangle")
(def-my-class group "attr-types/string" "attrs/group")
(def-my-class head_lp "attr-types/point" "attrs/head_lp")
(def-my-class headclip "attr-types/bool" "attrs/headclip")
(def-my-class headhref "attr-types/escString" "attrs/headhref")
(def-my-class headlabel "attr-types/lblString" "attrs/headlabel")
(def-my-class headport "attr-types/portPos" "attrs/headport")
(def-my-class headtarget "attr-types/escString" "attrs/headtarget")
(def-my-class headtooltip "attr-types/escString" "attrs/headtooltip")
(def-my-class height "attr-types/double" "attrs/height")
(def-my-class href "attr-types/escString" "attrs/href")
(def-my-class id "attr-types/escString" "attrs/id")
(def-my-class image "attr-types/string" "attrs/image")
(def-my-class imagepath "attr-types/string" "attrs/imagepath")
(def-my-class imagepos "attr-types/string" "attrs/imagepos")
(def-my-class imagescale "attr-types/bool attr-types/string" "attrs/imagescale")
(def-my-class inputscale "attr-types/double" "attrs/inputscale")
(def-my-class label "attr-types/lblString" "attrs/label")
(def-my-class label_scheme "attr-types/int" "attrs/label_scheme")
(def-my-class labelangle "attr-types/double" "attrs/labelangle")
(def-my-class labeldistance "attr-types/double" "attrs/labeldistance")
(def-my-class labelfloat "attr-types/bool" "attrs/labelfloat")
(def-my-class labelfontcolor "attr-types/color" "attrs/labelfontcolor")
(def-my-class labelfontname "attr-types/string" "attrs/labelfontname")
(def-my-class labelfontsize "attr-types/double" "attrs/labelfontsize")
(def-my-class labelhref "attr-types/escString" "attrs/labelhref")
(def-my-class labeljust "attr-types/string" "attrs/labeljust")
(def-my-class labelloc "attr-types/string" "attrs/labelloc")
(def-my-class labeltarget "attr-types/escString" "attrs/labeltarget")
(def-my-class labeltooltip "attr-types/escString" "attrs/labeltooltip")
(def-my-class landscape "attr-types/bool" "attrs/landscape")
(def-my-class layer "attr-types/layerRange" "attrs/layer")
(def-my-class layerlistsep "attr-types/string" "attrs/layerlistsep")
(def-my-class layers "attr-types/layerList" "attrs/layers")
(def-my-class layerselect "attr-types/layerRange" "attrs/layerselect")
(def-my-class layersep "attr-types/string" "attrs/layersep")
(def-my-class layout "attr-types/string" "attrs/layout")
(def-my-class len "attr-types/double" "attrs/len")
(def-my-class levels "attr-types/int" "attrs/levels")
(def-my-class levelsgap "attr-types/double" "attrs/levelsgap")
(def-my-class lhead "attr-types/string" "attrs/lhead")
(def-my-class lheight "attr-types/double" "attrs/lheight")
(def-my-class lp "attr-types/point" "attrs/lp")
(def-my-class ltail "attr-types/string" "attrs/ltail")
(def-my-class lwidth "attr-types/double" "attrs/lwidth")
(def-my-class margin "attr-types/double attr-types/point" "attrs/margin")
(def-my-class maxiter "attr-types/int" "attrs/maxiter")
(def-my-class mclimit "attr-types/double" "attrs/mclimit")
(def-my-class mindist "attr-types/double" "attrs/mindist")
(def-my-class minlen "attr-types/int" "attrs/minlen")
(def-my-class mode "attr-types/string" "attrs/mode")
(def-my-class model "attr-types/string" "attrs/model")
(def-my-class mosek "attr-types/bool" "attrs/mosek")
(def-my-class newrank "attr-types/bool" "attrs/newrank")
(def-my-class nodesep "attr-types/double" "attrs/nodesep")
(def-my-class nojustify "attr-types/bool" "attrs/nojustify")
(def-my-class normalize "attr-types/double attr-types/bool" "attrs/normalize")
(def-my-class notranslate "attr-types/bool" "attrs/notranslate")
(def-my-class nslimit "attr-types/double" "attrs/nslimit")
(def-my-class nslimit1 "attr-types/double" "attrs/nslimit1")
(def-my-class ordering "attr-types/string" "attrs/ordering")
(def-my-class orientation "attr-types/double attr-types/string" "attrs/orientation")
(def-my-class outputorder "attr-types/outputMode" "attrs/outputorder")
(def-my-class overlap "attr-types/string attr-types/bool" "attrs/overlap")
(def-my-class overlap_scaling "attr-types/double" "attrs/overlap_scaling")
(def-my-class overlap_shrink "attr-types/bool" "attrs/overlap_shrink")
(def-my-class pack "attr-types/bool attr-types/int attr-types/false" "attrs/pack")
(def-my-class packmode "attr-types/packMode" "attrs/packmode")
(def-my-class pad "attr-types/double attr-types/point" "attrs/pad")
(def-my-class page "attr-types/double attr-types/point" "attrs/page")
(def-my-class pagedir "attr-types/pagedir" "attrs/pagedir")
(def-my-class pencolor "attr-types/color" "attrs/pencolor")
(def-my-class penwidth "attr-types/double" "attrs/penwidth")
(def-my-class peripheries "attr-types/int" "attrs/peripheries")
(def-my-class pin "attr-types/bool" "attrs/pin")
(def-my-class pos "attr-types/point attr-types/splineType" "attrs/pos")
(def-my-class quadtree "attr-types/quadType attr-types/bool" "attrs/quadtree")
(def-my-class quantum "attr-types/double" "attrs/quantum")
(def-my-class rank "attr-types/rankType" "attrs/rank")
(def-my-class rankdir "attr-types/rankdir" "attrs/rankdir")
(def-my-class ranksep "attr-types/double attr-types/doubleList" "attrs/ranksep")
(def-my-class ratio "attr-types/double attr-types/string" "attrs/ratio")
(def-my-class rects "attr-types/rect" "attrs/rects")
(def-my-class regular "attr-types/bool" "attrs/regular")
(def-my-class remincross "attr-types/bool" "attrs/remincross")
(def-my-class repulsiveforce "attr-types/double" "attrs/repulsiveforce")
(def-my-class resolution "attr-types/double" "attrs/resolution")
(def-my-class root "attr-types/string attr-types/bool" "attrs/root")
(def-my-class rotate "attr-types/int" "attrs/rotate")
(def-my-class rotation "attr-types/double" "attrs/rotation")
(def-my-class samehead "attr-types/string" "attrs/samehead")
(def-my-class sametail "attr-types/string" "attrs/sametail")
(def-my-class samplepoints "attr-types/int" "attrs/samplepoints")
(def-my-class scale "attr-types/double attr-types/point" "attrs/scale")
(def-my-class searchsize "attr-types/int" "attrs/searchsize")
(def-my-class sep "attr-types/addDouble attr-types/addPoint" "attrs/sep")
(def-my-class shape "attr-types/shape" "attrs/shape")
(def-my-class shapefile "attr-types/string" "attrs/shapefile")
(def-my-class showboxes "attr-types/int" "attrs/showboxes")
(def-my-class sides "attr-types/int" "attrs/sides")
(def-my-class size "attr-types/double attr-types/point" "attrs/size")
(def-my-class skew "attr-types/double" "attrs/skew")
(def-my-class smoothing "attr-types/smoothType" "attrs/smoothing")
(def-my-class sortv "attr-types/int" "attrs/sortv")
(def-my-class splines "attr-types/bool attr-types/string" "attrs/splines")
(def-my-class start "attr-types/startType" "attrs/start")
(def-my-class style "attr-types/style" "attrs/style")
(def-my-class stylesheet "attr-types/string" "attrs/stylesheet")
(def-my-class tail_lp "attr-types/point" "attrs/tail_lp")
(def-my-class tailclip "attr-types/bool" "attrs/tailclip")
(def-my-class tailhref "attr-types/escString" "attrs/tailhref")
(def-my-class taillabel "attr-types/lblString" "attrs/taillabel")
(def-my-class tailport "attr-types/portPos" "attrs/tailport")
(def-my-class tailtarget "attr-types/escString" "attrs/tailtarget")
(def-my-class tailtooltip "attr-types/escString" "attrs/tailtooltip")
(def-my-class target "attr-types/escString attr-types/string" "attrs/target")
(def-my-class tooltip "attr-types/escString" "attrs/tooltip")
(def-my-class truecolor "attr-types/bool" "attrs/truecolor")
(def-my-class vertices "attr-types/pointList" "attrs/vertices")
(def-my-class viewport "attr-types/viewPort" "attrs/viewport")
(def-my-class voro_margin "attr-types/double" "attrs/voro_margin")
(def-my-class weight "attr-types/int attr-types/double" "attrs/weight")
(def-my-class width "attr-types/double" "attrs/width")
(def-my-class xdotversion "attr-types/string" "attrs/xdotversion")
(def-my-class xlabel "attr-types/lblString" "attrs/xlabel")
(def-my-class xlp "attr-types/point" "attrs/xlp")
(def-my-class z "attr-types/double" "attrs/z")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass <node-attributes>
    (<area> <class> <color> <colorscheme> <comment> <distortion>
     <fillcolor> <fixedsize> <fontcolor> <fontname> <fontsize>
     <gradientangle> <group> <height> <href> <id> <image> <imagepos>
     <imagescale> <label> <labelloc> <layer> <margin> <nojustify>
     <ordering> <orientation> <penwidth> <peripheries> <pin> <pos>
     <rects> <regular> <root> <samplepoints> <shape> <shapefile>
     <showboxes> <sides> <skew> <sortv> <style> <target> <tooltip>
     <vertices> <width> <xlabel> <xlp> <z>)
  ()
  (:documentation "@link[uri=\"https://graphviz.org/doc/info/attrs.html\"](Attributes)"))

(defclass <edge-attributes>
    (<arrowhead> <arrowsize> <arrowtail> <class> <color> <colorscheme>
     <comment> <constraint> <decorate> <dir> <edgehref> <edgetarget>
     <edgetooltip> <fillcolor> <fontcolor> <fontname> <fontsize> <head_lp>
     <headclip> <headhref> <headlabel> <headport> <headtarget>
     <headtooltip> <href> <id> <label> <labelangle> <labeldistance>
     <labelfloat> <labelfontcolor> <labelfontname> <labelfontsize>
     <labelhref> <labeltarget> <labeltooltip> <layer> <len> <lhead> <lp>
     <ltail> <minlen> <nojustify> <penwidth> <pos> <samehead> <sametail>
     <showboxes> <style> <tail_lp> <tailclip> <tailhref> <taillabel>
     <tailport> <tailtarget> <tailtooltip> <target> <tooltip> <weight>
     <xlabel> <xlp> )
  ()
  (:documentation "@link[uri=\"https://graphviz.org/doc/info/attrs.html\"](Attributes)"))

(defclass <graph-attributes>
    (<bb> <bgcolor> <center> <charset> <class> <clusterrank>
     <colorscheme> <comment> <compound> <concentrate> <defaultdist>
     <dim> <dimen> <diredgeconstraints> <dpi> <epsilon> <esep>
     <fontcolor> <fontname> <fontnames> <fontpath> <fontsize>
     <forcelabels> <gradientangle> <href> <id> <imagepath>
     <inputscale> <label> <label_scheme> <labeljust> <labelloc>
     <landscape> <layerlistsep> <layers> <layerselect> <layersep>
     <layout> <levels> <levelsgap> <lheight> <lp> <lwidth> <margin>
     <maxiter> <mclimit> <mindist> <mode> <model> <mosek> <newrank>
     <nodesep> <nojustify> <normalize> <notranslate> <nslimit>
     <nslimit1> <ordering> <orientation> <outputorder> <overlap>
     <overlap_scaling> <overlap_shrink> <pack> <packmode> <pad> <page>
     <pagedir> <quadtree> <quantum> <rankdir> <ranksep> <ratio>
     <remincross> <repulsiveforce> <resolution> <root> <rotate>
     <rotation> <scale> <searchsize> <sep> <showboxes> <size>
     <smoothing> <sortv> <splines> <start> <style> <stylesheet>
     <target> <tooltip> <truecolor> <viewport> <voro_margin>
     <xdotversion> )
  ()
  (:documentation "@link[uri=\"https://graphviz.org/doc/info/attrs.html\"](Attributes)"))

(defclass <graph-attributes>
    (<arrowhead> <arrowsize> <arrowtail> <class> <color> <colorscheme>
     <comment> <constraint> <decorate> <dir> <edgehref> <edgetarget>
     <edgetooltip> <fillcolor> <fontcolor> <fontname> <fontsize>
     <head_lp> <headclip> <headhref> <headlabel> <headport>
     <headtarget> <headtooltip> <href> <id> <label> <labelangle>
     <labeldistance> <labelfloat> <labelfontcolor> <labelfontname>
     <labelfontsize> <labelhref> <labeltarget> <labeltooltip> <layer>
     <len> <lhead> <lp> <ltail> <minlen> <nojustify> <penwidth> <pos>
     <samehead> <sametail> <showboxes> <style> <tail_lp> <tailclip>
     <tailhref> <taillabel> <tailport> <tailtarget> <tailtooltip>
     <target> <tooltip> <weight> <xlabel> <xlp> )
  ()
  (:documentation "@link[uri=\"https://graphviz.org/doc/info/attrs.html\"](Attributes)"))

(defclass <subgraphs-attributes> (<rank>)
  ()
  (:documentation "@link[uri=\"https://graphviz.org/doc/info/attrs.html\"](Attributes)"))

(defclass <cluster-attributes>
    (<area> <bgcolor> <class> <color> <colorscheme> <fillcolor>
     <fontcolor> <fontname> <fontsize> <gradientangle> <href> <id>
     <label> <labeljust> <labelloc> <layer> <lheight> <lp> <lwidth>
     <margin> <nojustify> <pencolor> <penwidth> <peripheries> <sortv>
     <style> <target> <tooltip> )
  ()
  (:documentation "@link[uri=\"https://graphviz.org/doc/info/attrs.html\"](Attributes)"))
