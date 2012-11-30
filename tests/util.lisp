(in-package #:spasm-tests)

(use-package :spasm)


;;; unit tests for util.concat
;;;
(deftestsuite concat-test-case (spasm-test-suite) ())

(addtest (concat-test-case)
  test-concat
  (ensure-same "abc" (concat "a" "b" "c")))
