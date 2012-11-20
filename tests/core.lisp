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
  test-non-container-tags
  (ensure-same "<br />" (html (:br)))
  (ensure-same "<img src='loldogs.jpg' />" (html (:img :src="loldogs.jpg"))))

(addtest (html-test-case)
  test-container-tags
  (ensure-same "<span></span>" (html (:span))))

(addtest (html-test-case)
  test-content
  (ensure-same "<p>How YOU doin'?</p>" (html (:p "How YOU doin'?")))
  (ensure-same
    "<p class='question'>How YOU doin'?</p>"
    (html (:p :class "question" "How YOU doin'?"))))

(addtest (html-test-case)
  test-css-id-and-class
  (ensure-same
    "<div id='foo' class='bar baz'>bang</div>"
    (html (:div#foo.bar.baz "bang"))))
