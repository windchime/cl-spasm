;;; This file can simply be loaded from the sbcl interpreter like so:
;;;
;;; * (load "tests/core.lisp")
;;;
(ql:quickload 'xlunit)


(defpackage #:spasm-tests
  (:use #:cl #:xlunit #:spasm))

(in-package #:spasm-tests)

(defclass regex-test-case (test-case)
  ())

(def-test-method test-XXX ((test html-test-case))
  ())
