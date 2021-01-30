;;;; tests/package.lisp

(defpackage #:mnas-graph/tests
  (:use #:cl #:fiveam)
  (:export #:run-tests))

(in-package :mnas-graph/tests)

(defun run-tests () (run! 'all))

;;;;(run-tests)
