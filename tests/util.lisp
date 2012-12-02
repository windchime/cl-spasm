(in-package #:spasm-tests)

(use-package :spasm)


;;; unit tests for util.concat
;;;
(deftestsuite concat-test-case (spasm-test-suite) ())

(addtest (concat-test-case)
  test-concat
  (ensure-same "" (concat))
  (ensure-same "a" (concat "a"))
  (ensure-same "abc" (concat "a" "b" "c")))


;;; unit tests for util.*-pair
;;;
(deftestsuite pairs-test-case (spasm-test-suite) ())

(addtest (pairs-test-case)
  test-car-pair
    (ensure-same '(:a . 1) (car-pair '(:a 1 :b 2 :c 3 :d 4 :e 5)))
    (ensure-same '(:a . 1) (car-pair '(:a 1)))
    (ensure-same '(nil) (car-pair '())))

(addtest (pairs-test-case)
  test-cdr-pairs
    (ensure-same
      '(:b 2 :c 3 :d 4 :e 5)
      (cdr-pairs '(:a 1 :b 2 :c 3 :d 4 :e 5)))
    (ensure-same nil (cdr-pairs '(:a 1)))
    (ensure-same nil (cdr-pairs '())))

(addtest (pairs-test-case)
  test-get-pairs
    (ensure-same
      '((:a . 1) (:b . 2) (:c . 3) (:d . 4) (:e . 5))
      (get-pairs '(:a 1 :b 2 :c 3 :d 4 :e 5)))
    (ensure-same '(:a . 1) (get-pairs '(:a 1)))
    (ensure-same '(nil) (car-pair '())))
