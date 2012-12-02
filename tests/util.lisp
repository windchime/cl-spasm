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
      '((:a 1) (:b 2) (:c 3) (:d 4) (:e 5))
      (get-pairs '(:a 1 :b 2 :c 3 :d 4 :e 5)))
    (ensure-same '((:a 1)) (get-pairs '(:a 1)))
    (ensure-same nil (get-pairs '())))


;;; unit tests for util.merge-plists
;;;
(deftestsuite merge-plists-test-case (spasm-test-suite) ())

(addtest (merge-plists-test-case)
  test-simple
    (ensure-same '() (merge-plists '() '()))
    (ensure-same '(:a 1) (merge-plists '(:a 1) '()))
    (ensure-same '(:a 1 :b 2) (merge-plists '(:a 1) '(:b 2)))
    (ensure-same
      '(:b 2 :a 1 :c 3 :d 4)
      (merge-plists '(:a 1 :b 2) '(:c 3 :d 4)))
    (ensure-same
      '(:c 3 :b 2 :a 1 :d 4 :e 5)
      (merge-plists '(:a 1 :b 2 :c 3) '(:d 4 :e 5))))

(addtest (merge-plists-test-case)
  test-overlapping-keys
    (ensure-same '(:a 2) (merge-plists '(:a 1) '(:a 2)))
    (ensure-same
      '(:a 3 :b 4)
      (merge-plists '(:a 1 :b 2) '(:a 3 :b 4)))
    (ensure-same
      '(:b 2 :a 3 :c 4)
      (merge-plists '(:a 1 :b 2) '(:a 3 :c 4)))
    (ensure-same
      '(:a 1 :b 3 :c 4)
      (merge-plists '(:a 1 :b 2) '(:b 3 :c 4)))
    (ensure-same
      '(:a 1 :c 3 :b 4)
      (merge-plists '(:a 1 :b 2) '(:c 3 :b 4)))
    (ensure-same
      '(:b 2 :a 4 :c 5)
      (merge-plists '(:a 1 :b 2 :c 3) '(:a 4 :c 5))))


;;; unit tests for util.ends-in-pair?
;;;
(deftestsuite ends-in-pair-test-case (spasm-test-suite) ())

(addtest (ends-in-pair-test-case)
  test-true
    (ensure-same t (ends-in-pair? '(:a 1)))
    (ensure-same t (ends-in-pair? '(:a 1 :b 2)))
    (ensure-same t (ends-in-pair? '(:a 1 :b 2 :c 3)))
    (ensure-same t (ends-in-pair? '(:a 1 2 3 4 :c 5)))
    (ensure-same t (ends-in-pair? '(1 2 3 4 :c 5))))

(addtest (ends-in-pair-test-case)
  test-false
    (ensure-same nil (ends-in-pair? '()))
    (ensure-same nil (ends-in-pair? '(1 2)))
    (ensure-same nil (ends-in-pair? '(1 2 3)))
    (ensure-same nil (ends-in-pair? '(:a 1 2)))
    (ensure-same nil (ends-in-pair? '(:a 1 2 3)))
    (ensure-same nil (ends-in-pair? '(:a))))
    ;)
