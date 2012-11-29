(in-package #:spasm-tests)

(use-package :spasm)


;;; unit tests for util.coerce-tag
;;;
(deftestsuite coerce-tag-test-case (spasm-test-suite) ())

(addtest (coerce-tag-test-case)
  test-string
  (ensure-same
    "div"
    (coerce-tag "div")))

(addtest (coerce-tag-test-case)
  test-symbol
  (ensure-same
    "div"
    (coerce-tag :div)))


;;; unit tests for util.parse-initial-tag
;;;
(deftestsuite parse-initial-tag-test-case (spasm-test-suite) ())

;; without the keyword parameter, return values
(addtest (parse-initial-tag-test-case)
  test-tag-only
  (ensure-same
    '("div" nil nil)
    (multiple-value-list (parse-initial-tag :div))))

(addtest (parse-initial-tag-test-case)
  test-tag-and-id
  (ensure-same
    '("div" "cssid" nil)
    (multiple-value-list (parse-initial-tag :div#cssid))))

(addtest (parse-initial-tag-test-case)
  test-tag-id-one-class
  (ensure-same
    '("div" "cssid" "class")
    (multiple-value-list (parse-initial-tag :div#cssid.class))))

(addtest (parse-initial-tag-test-case)
  test-tag-id-classes
  (ensure-same
    '("div" "cssid" "class1 class2 class3")
    (multiple-value-list
      (parse-initial-tag :div#cssid.class1.class2.class3))))

;; with the keyword parameter, return a list
(addtest (parse-initial-tag-test-case)
  test-tag-only-with-keyword-arg
  (ensure-same
    '("div" nil nil)
    (parse-initial-tag :div :as-list t)))

(addtest (parse-initial-tag-test-case)
  test-tag-and-id-with-keyword-arg
  (ensure-same
    '("div" "cssid" nil)
    (parse-initial-tag :div#cssid :as-list t)))

(addtest (parse-initial-tag-test-case)
  test-tag-id-one-class-with-keyword-arg
  (ensure-same
    '("div" "cssid" "class")
    (parse-initial-tag :div#cssid.class :as-list t)))

(addtest (parse-initial-tag-test-case)
  test-tag-id-classes-with-keyword-arg
  (ensure-same
    '("div" "cssid" "class1 class2 class3")
    (parse-initial-tag :div#cssid.class1.class2.class3 :as-list t)))


;;; unit tests for util.normalize-element
;;;
(deftestsuite normalize-element-test-case (spasm-test-suite) ())

(addtest (normalize-element-test-case)
  test-just-tag
  (ensure-same "" (normalize-element '(:p))))

(addtest (normalize-element-test-case)
  test-tag-and-content
  (ensure-same "" (normalize-element '(:p "some text")))
  (ensure-same "" (normalize-element '(:p :class "some-style" "some text")))
  (ensure-same
    ""
    (normalize-element '(:p :id "p0" :class "some-style" "some text"))))

(addtest (normalize-element-test-case)
  test-tag-no-content
  (ensure-same
    ""
    (normalize-element '(:img :src "./loldogs.jpg")))
  (ensure-same
    ""
    (normalize-element '(:img :src "./loldogs.jpg" :class "my-images"))))
