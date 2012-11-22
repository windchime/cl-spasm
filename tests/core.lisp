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

;; test case for compile-* functions and methods
(deftestsuite compile-test-case (spasm-test-suite) ())

(addtest (compile-test-case)
  test-attr
  (ensure-same " src='./loldogs.jpg'" (compile-attr "src" "./loldogs.jpg")))

;; test case for the html (macro? function? dunno yet...)
;(deftestsuite html-test-case (spasm-test-suite) ())
(deftestsuite html-test-case () ())

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
  test-css-syntactic-sugar
  (ensure-same "<div id='foo'></div>" (html (:div#foo)))
  (ensure-same "<div class='bar'>" (html (:div.bar)))
  (ensure-same "<div class='bar baz'>" (html (:div.bar.baz)))
  (ensure-same "<div id='foo' class='bar baz'>" (html (:div#foo.bar.baz)))
  (ensure-same
    "<div id='foo' class='bar baz'>bang</div>"
    (html (:div#foo.bar.baz "bang"))))
