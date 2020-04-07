;;;; package.lisp


(defpackage #:mnas-graph
  (:use #:cl #:mnas-hash-table))

;;;; (declaim (optimize (compilation-speed 0) (debug 3) (safety 0) (space 0) (speed 0)))

(setf sb-impl::*default-external-format* :utf8)
