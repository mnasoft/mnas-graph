(in-package #:mnas-graph)

(defparameter *code* nil)

(defun load-lisp-lisp-as-code (fname &key (temporary-fname "~/quicklisp/local-projects/mnas/mnas-graph/temp-mnas-graph.lisp") )
  (with-open-file (o-stream temporary-fname :direction :output :if-exists :supersede )
    (princ "(defparameter *code* '(" o-stream)
    (princ #\newline o-stream)
    (with-open-file (stream fname )
      (loop for line = (read-line stream nil 'foo)
	 until (eq line 'foo)
	 do (progn
	      (princ line o-stream)
	      (princ #\newline o-stream))))
    (princ #\newline o-stream)
    (princ "))" o-stream))
  (load temporary-fname)
  *code*)

(load-lisp-lisp-as-code "~/quicklisp/local-projects/mnas/mnas-graph/mnas-graph.lisp")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun defun-code (code)
  (let ((rez nil))
    (mapc
     #'(lambda (el)
	 (when  (eql (car el) 'defun)
	   (push el rez)))
     code)
    (reverse rez)))

(defun defun-name (code)
  (remove-duplicates (mapcar 'second (defun-code code))))

(defun defmethod-code (code)
  (let ((rez nil))
    (mapc
     #'(lambda (el)
	 (when  (eql (car el) 'defmethod)
	   (push el rez)))
     code)
    (reverse rez)))

(defun defmethod-name (code)
  (remove-duplicates (mapcar 'second (defmethod-code code))))

(defun def-name (code)
  (append (defun-name code) (defmethod-name code)))

(defun-name *code*)
(defmethod-name *code*)
(def-name *code*)

(defun-code *code*)

