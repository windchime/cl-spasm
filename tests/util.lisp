;;; This file can simply be loaded from the sbcl interpreter like so:
;;;
;;; * (load "tests/core.lisp")
;;;
(in-package #:spasm-tests)

(use-package :spasm)

(deftestsuite parse-initial-tag-test-case (spasm-test-suite) ())

(addtest (parse-initial-tag-test-case)
  test-tag-only
  (ensure-same
    '(":div" nil nil)
    (parse-initial-tag ":div")))

(addtest (parse-initial-tag-test-case)
  test-tag-and-id
  (ensure-same
    '(":div" "cssid" nil)
    (parse-initial-tag ":div#cssid")))

(addtest (parse-initial-tag-test-case)
  test-tag-id-one-class
  (ensure-same
    '(":div" "cssid" "class")
    (parse-initial-tag ":div#cssid.class")))

(addtest (parse-initial-tag-test-case)
  test-tag-id-classes
  (ensure-same
    '(":div" "cssid" "class1.class2.class3")
    (parse-initial-tag ":div#cssid.class1.class2.class3")))
