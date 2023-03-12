;;;; tests/package.lisp

(defpackage :mnas-graph/tests
  (:use #:cl #:fiveam)
  (:export #:run-tests))

(in-package :mnas-graph/tests)

(defun run-tests () (run! 'all))

(def-suite all
  :description "Мастер-набор всех тестов проекта mnas-graph.")

(in-suite all)

;;;;(run-tests)
