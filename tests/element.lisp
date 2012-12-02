(in-package #:spasm-tests)

(use-package 'spasm)

;;; unit tests for element.container-tag(s)
;;;
(deftestsuite container-tags-test-case (spasm-test-suite) ())

(addtest (container-tags-test-case)
  test-container-tags
  (ensure-different nil (member ':a *container-tags*))
  (ensure-different nil (member ':div *container-tags*))
  (ensure-different nil (member ':option *container-tags*))
  (ensure-different nil (member ':ul *container-tags*)))

(addtest (container-tags-test-case)
  test-non-container-tags
  (ensure-same nil (member ':br *container-tags*))
  (ensure-same nil (member ':hl *container-tags*))
  (ensure-same nil (member ':img *container-tags*)))

(addtest (container-tags-test-case)
  test-container-tag-true
  (ensure-same t (container-tag? ':a))
  (ensure-same t (container-tag? ':div))
  (ensure-same t (container-tag? ':option))
  (ensure-same t (container-tag? ':ul)))

(addtest (container-tags-test-case)
  test-container-tag-false
  (ensure-same nil (container-tag? ':br))
  (ensure-same nil (container-tag? ':hl))
  (ensure-same nil (container-tag? ':img)))


;;; unit tests for element.html-attr
;;;
(deftestsuite html-attr-test-case (spasm-test-suite) ())

(addtest (html-attr-test-case)
  test-html-attr
  (ensure-same " src='./loldogs.jpg'" (html-attr "src" "./loldogs.jpg"))
  (ensure-same
    " src='&lt;whacky name&gt;.jpg'"
    (html-attr "src" "<whacky name>.jpg")))


;;; unit tests for element.make-attrs
;;;
(deftestsuite make-attrs-test-case (spasm-test-suite) ())

(addtest (make-attrs-test-case)
  test-make-attrs-list-of-plists
  (ensure-same "" (make-attrs nil))
  (ensure-same " a='1'" (make-attrs '((:a "1"))))
  (ensure-same " a='1' b='2'" (make-attrs '((:a "1") (:b "2"))))
  (ensure-same
    " a='1' b='2' c='3' d='4' e='5'"
    (make-attrs '((:a "1") (:b "2") (:c "3") (:d "4") (:e "5")))))

(addtest (make-attrs-test-case)
  test-make-attrs-plist
  (ensure-same "" (make-attrs nil))
  (ensure-same " a='1'" (make-attrs '(:a "1")))
  (ensure-same " a='1' b='2'" (make-attrs '(:a "1" :b "2")))
  (ensure-same
    " a='1' b='2' c='3' d='4' e='5'"
    (make-attrs '(:a "1" :b "2" :c "3" :d "4" :e "5"))))


;;; unit tests for element.make-tag
;;;
(deftestsuite make-tag-test-case (spasm-test-suite) ())

(addtest (make-tag-test-case)
  test-make-tag-opening
  (ensure-same "<section>" (make-tag "section")))

(addtest (make-tag-test-case)
  test-make-tag-empty
  (ensure-same "<br />" (make-tag "br" :empty t)))

(addtest (make-tag-test-case)
  test-make-with-attrs
  (ensure-same "<img src='./'>" (make-tag "img" :attrs '(:src "./"))))

(addtest (make-tag-test-case)
  test-make-tag-closing
  (ensure-same "</section>" (make-tag "section" :closing t)))


;;; unit tests for element.coerce-key
;;;
(deftestsuite coerce-key-test-case (spasm-test-suite) ())

(addtest (coerce-key-test-case)
  test-string
  (ensure-same "div" (coerce-key "div"))
  (ensure-same "div" (coerce-key :div)))


;;; unit tests for element.parse-initial-tag
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


;;; unit tests for element.normalize-element
;;;
(deftestsuite normalize-element-test-case (spasm-test-suite) ())

(addtest (normalize-element-test-case)
  test-just-tag
  (ensure-same "" (normalize-element '(:p) :as-list t)))

(addtest (normalize-element-test-case)
  test-tag-and-content
  (ensure-same "" (normalize-element '(:p "some text") :as-list t))
  (ensure-same
    ""
    (normalize-element '(:p :class "some-style" "some text") :as-list t))
  (ensure-same
    ""
    (normalize-element
      '(:p :id "p0" :class "some-style" "some text")
      :as-list t)))

(addtest (normalize-element-test-case)
  test-tag-no-content
  ;(ensure-same
  ;  ""
  ;  (normalize-element '(:img :src "./loldogs.jpg") :as-list t))
  ;(ensure-same
  ;  ""
  ;  (normalize-element
  ;    '(:img :src "./loldogs.jpg" :class "my-images")
  ;    :as-list t))
  (ensure-same
    ""
    (normalize-element
      '(:div#cssid.class1.class2.class3)
      :as-list t)))


;;; unit tests for element.make-element
;;;
(deftestsuite make-element-test-case (spasm-test-suite) ())

(addtest (make-element-test-case)
  test-make-element
  (ensure-same "<p />" (make-element "p"))
  (ensure-same
    "<p class='section' />"
    (make-element "p" :attrs '(:class "section")))
  (ensure-same
    "<p>some content</p>"
    (make-element "p" :content "some content"))
  (ensure-same
    "<p class='section'>some content</p>"
    (make-element "p" :attrs '(:class "section") :content "some content")))
