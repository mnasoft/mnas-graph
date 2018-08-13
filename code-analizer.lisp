(in-package #:mnas-graph)

(setf sb-impl::*default-external-format* :UTF-8)

(defun load-lisp-lisp-as-code (fname &key (temporary-fname "~/quicklisp/local-projects/mnas/mnas-graph/tem-mnas-graph.lisp"))
  (with-open-file (o-stream temporary-fname :direction :output :if-exists :supersede :external-format :UTF-8) ;;;; 
    (princ "(defparameter *code* '(" o-stream)
    (princ #\newline o-stream)
    (with-open-file (stream fname :external-format  :UTF-8) ;;;; 
      (loop for line = (read-line stream nil 'foo)
	 until (eq line 'foo)
	 do (progn
	      (princ line o-stream)
	      (princ #\newline o-stream))))
    (princ #\newline o-stream)
    (princ "))" o-stream))
  (load temporary-fname)
  *code*)

(defparameter *code* 
  (load-lisp-lisp-as-code "~/quicklisp/local-projects/mnas/mnas-graph/mnas-graph.lisp"))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *c* nil)

(defun ignore-header (lst)
  (if (not (listp (car lst)))
      (ignore-header (cdr lst))
      (progn (setf lst (cdr lst))
	     (when (stringp (car lst))
	       (setf lst (cdr lst))))))

(defun find-first (lst)
  (cond
    ((null lst) *c*)
     (null (car lst)) *c*)
      (push (car lst) *c*)
      (setf lst (cdr lst))
      ))

(find-first (ignore-header (nth 92 *code*)))

(defparameter *ttt* '((GENERATE-GRAPH
  (LET ((LST NIL))
    (DOTIMES (I EDGES-NUMBER LST)
      (PUSH
       (LIST (FORMAT NIL "~A" (RANDOM NODE-MAX-NUMBER))
             (FORMAT NIL "~A" (RANDOM NODE-MAX-NUMBER)))
       LST))))))

(when (and (car *ttt*) (symbolp (car *ttt*))) t)

(when (and (car *ttt*) (listp (car *ttt*))) (setf *ttt* (car *ttt*)))


'(DEFUN MAKE-RANDOM-GRAPH
    (&KEY (NODE-MAX-NUMBER 100) (EDGES-NUMBER NODE-MAX-NUMBER))
  (GENERATE-GRAPH
   (LET ((LST NIL))
     (DOTIMES (I EDGES-NUMBER LST)
       (PUSH
        (LIST (FORMAT NIL "~A" (RANDOM NODE-MAX-NUMBER))
              (FORMAT NIL "~A" (RANDOM NODE-MAX-NUMBER)))
        LST)))))

(export 'coool)
(defun coool () 
  (view-graph
   (generate-graph
    '(("Начало" "Исключить не списки")
      ("Исключить не списки" 
      ))))

(coool)
