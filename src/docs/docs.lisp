
(defpackage :mnas-graph/docs
  (:use #:cl ) 
  (:nicknames "MGRAPH/DOCS")
  (:export make-all)
  (:documentation "Пакет @b(mnas-graph/docs) содержит функции
  генерирования и публикации документации."))

(in-package :mnas-graph/docs)

(defun make-document ()
  (loop
    :for j :from 1
    :for i :in
    '((:mnas-graph                :mnas-graph)
      (:mnas-graph/filter         nil)
      (:mnas-graph/printer-viewer nil)
      (:mnas-graph/view           nil)
      (:mnas-graph/demos          nil)
      )
    :do
       (progn
         (apply #'mnas-package:document i)
         (format t "~A ~A~%" j i))))

(defun make-graphs ()
  (loop
    :for j :from 1
    :for i :in
    '(:mnas-graph
      :mnas-graph/filter
      :mnas-graph/printer-viewer
      :mnas-graph/view
      :mnas-graph/demos)
    :do (progn
          (mnas-package:make-codex-graphs i i)
          (format t "~A ~A~%" j i))))

(defun make-all (&aux
                   (of (if (find (uiop:hostname)
                                 mnas-package:*intranet-hosts*
                                 :test #'string= :key #'first)
                           '(:type :multi-html :template :gamma)
                           '(:type :multi-html :template :minima))))
  (let* ((sys-symbol :mnas-graph)
         (sys-string (string-downcase (format nil "~a" sys-symbol))))
    (mnas-package:make-html-path sys-symbol)
    (make-document)
    (mnas-package:make-mainfest-lisp `(,sys-symbol)
                                     (string-capitalize sys-string)
                                     '("Mykola Matvyeyev")
                                     (mnas-package:find-sources sys-symbol)
                                     :output-format of)
    (codex:document sys-symbol)
    (make-graphs)
    (mnas-package:copy-doc->public-html sys-string)
    (mnas-package:rsync-doc sys-string)
    :make-all-finish))

;;;; (make-all)
