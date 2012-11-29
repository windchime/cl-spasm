(in-package #:spasm)

(defvar *regex-id-class*
  "([^ .#]+)(?:#([^ .#]+))?(?:\.([^ #]+))?")

(defun coerce-tag (tag)
  (cond ((symbolp tag) (string-downcase (symbol-name tag)))
        (t tag)))

(defun parse-initial-tag (tag &key as-list)
  "Given an XHTML tag, parse it for CSS id and/or classes.

  Returns a list of (tag-name css-id and css-classes) where the CSS classes are
  dot-separated."
  (let ((tag-name nil)
        (id nil)
        (classes nil))
    (cl-ppcre:do-register-groups (parsed-tag-name parsed-id parsed-classes)
      (*regex-id-class* (coerce-tag tag))
      (setf tag-name parsed-tag-name
            id parsed-id
            classes (substitute #\Space #\. parsed-classes)))
    ; XXX change this to a (cond)
    (if (eql as-list t)
      ; if a list is required, return that now
      (return-from parse-initial-tag (list tag-name id classes))
      ; otherwise, just return the values (the default behaviour)
      (values tag-name id classes))))

;(defun normalize-element (body &key as-list)
;  (multiple-value-bind
;    (tag id classes) (parse-initial-tag (first body) :as-list as-list)
;    (let ((tag-attrs (list :id id :class classes)))
;         ((map-attrs (first (second body))))
;      (if map-attrs
;        (values tag (append tag-attrs map-attrs) (second (second (body))))
;        (values tag tag-attrs body)))))

(defun normalize-element (body &key as-list)
  (multiple-value-bind
    (tag id classes) (parse-initial-tag (first body) :as-list as-list)
         (print "body: " body)
         (print "tag " tag)
         (print "id " id)
         (print "classes " classes)
         ))
