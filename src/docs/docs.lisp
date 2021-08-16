(defpackage #:mnas-graph/docs
  (:use #:cl ) 
  (:nicknames "MGRAPH/DOCS")
  (:export make-all)
  (:documentation "Пакет @b(mnas-graph/docs) содержит функции
  генерирования и публикации документации."))

(in-package :mnas-graph/docs)

(defun make-document ()
  (loop
    :for i :in
    '((:mnas-graph                :mnas-graph)
      (:mnas-graph/filter         nil)
      (:mnas-graph/printer-viewer nil)
      (:mnas-graph/view           nil)
      (:mnas-graph/demos          nil)
      )
    :do (apply #'mnas-package:document i)))

(defun make-graphs ()
  (loop
    :for i :in
    '(:mnas-graph
      :mnas-graph/filter
      :mnas-graph/printer-viewer
      :mnas-graph/view
      :mnas-graph/demos)
    :do (mnas-package:make-codex-graphs i i)))

(defun make-all (&aux
                   (of (if (find (uiop:hostname)
                                 mnas-package:*intranet-hosts*
                                 :test #'string=)
                           '(:type :multi-html :template :gamma)
                           '(:type :multi-html :template :minima))))
  "@b(Описание:) функция @b(make-all) служит для создания документации.

 Пакет документации формируется в каталоге
~/public_html/Common-Lisp-Programs/mnas-graph.
"
  (mnas-package:make-html-path :mnas-graph)
  (make-document)
  (make-graphs)
  (mnas-package:make-mainfest-lisp
   '(:mnas-graph :mnas-graph/docs)
   "Mnas-Graph"
   '("Nick Matvyeyev")
   (mnas-package:find-sources "mnas-graph")
   :output-format of)
  (codex:document :mnas-graph)
  (make-graphs)
  (mnas-package:copy-doc->public-html "mnas-graph")
  (mnas-package:rsync-doc "mnas-graph"))

;;;; (make-all)
