;;; This file can simply be loaded from the sbcl interpreter like so:
;;;
;;; * (load "tests/core.lisp")
;;;
(defpackage #:spasm-tests
  (:use #:cl #:lift #:spasm)
  (:export #:run-spasm-tests))

(in-package #:spasm-tests)

(deftestsuite spasm-test-suite () ())

(defun run-spasm-tests (&optional (suite 'spasm-test-suite))
  (let ((*test-print-testsuite-names* t)
        (*test-print-test-case-names* t)
        (*lift-debug-output* t)
        (*test-describe-if-not-successful?* t))

    (describe (run-tests :suite suite
                         :report-pathname nil))))

(deftestsuite html-test-case (spasm-test-suite) ())

(addtest (html-test-case)
  test-XXX
  (ensure-same "a" "a"))
