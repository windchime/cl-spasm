(in-package #:spasm)

(defvar *regex-id-class*
  "([^ .#]+)(?:#([^ .#]+))?(?:\.([^ #]+))?"
  "
  A regular expression for splitting a tag by CSS id and CSS class name.
  ")

(defvar *container-tags*
  '(:a :article :aside :b :body :canvas :dd :div :dl :dt :em :fieldset :footer
       :form :h1 :h2 :h3 :h4 :h5 :h6 :head :header :hgroup :html :i :iframe
       :label :li :nav :ol :option :pre :section :script :span :strong :style
       :table :textarea :title :ul)
  "
  A list of elements that need an explicit ending tag when rendered.
  ")

(defun container-tag? (tag)
  "
  A convenience function for testing wheter a tag is a container tag or not.
  "
  (cond ((member tag *container-tags*) t)))

(defun coerce-key (key)
  "
  A utility function for getting the string value of a key.

  In particular, this allows keyword symbols (e.g., :some-key) to be used as
  strings in other parts of the code.
  "
  (cond ((symbolp key) (string-downcase (symbol-name key)))
        (t key)))

(defun html-attr (key datum)
  "
  Given a key/datum pair, create an HTML/XML attribute.
  "
  (concat " " (coerce-key key) "='"
          (html-entities:encode-entities datum) "'"))

(defun make-attrs (attrs)
  "
  Given a plist key/datum pairs (a list of plists), create a string of HTML
  attributes.

  The attrs is first checked to see if it's a plist or if the plist has
  already been partitioned into a list of plists.
  "
  (cond ((keywordp (car attrs))
        (setf attrs (get-pairs attrs))))
  (apply 'concat
         (mapcar #'(lambda (x) (apply 'html-attr x)) attrs)))

(defun make-tag (tag-name &key attrs closing empty)
  "
  Make an opening or closing HTML tag.
  "
  (cond ((and empty attrs) (concat "<" tag-name (make-attrs attrs) " />"))
        (attrs (concat "<" tag-name (make-attrs attrs) ">"))
        (closing (concat "</" tag-name ">"))
        (empty (concat "<" tag-name " />"))
        (t (concat "<" tag-name ">"))))

(defun parse-initial-tag (tag &key as-list)
  "
  Given an XHTML tag, parse it for CSS id and/or classes.

  Returns a list of (tag-name css-id and css-classes) where the CSS classes are
  dot-separated.
  "
  (let ((tag-name nil)
        (id nil)
        (classes nil))
    (cl-ppcre:do-register-groups (parsed-tag-name parsed-id parsed-classes)
      (*regex-id-class* (coerce-key tag))
      (setf tag-name parsed-tag-name
            id parsed-id
            classes (substitute #\Space #\. parsed-classes)))
    ; XXX change this to a (cond)
    (if (eql as-list t)
      ; if a list is required, return that now
      (return-from parse-initial-tag (list tag-name id classes))
      ; otherwise, just return the values (the default behaviour)
      (values tag-name id classes))))

(defun make-element (tag-name &key attrs content)
  ""
  (cond (content ())
        ((container-tag? tag-name) (make-tag tag-name :attrs attrs)
                                   (make-tag tag-name :closing t))
        (t (make-tag tag-name :attrs attrs :empty t))))

;(defun normalize-element (body &key as-list)
;  (multiple-value-bind
;    (tag id classes) (parse-initial-tag (first body) :as-list as-list)
;    (let ((tag-attrs (list :id id :class classes)))
;         ((map-attrs (first (second body))))
;      (if map-attrs
;        (values tag (append tag-attrs map-attrs) (second (second (body))))
;        (values tag tag-attrs body)))))

(defun normalize-element (body &key as-list)
  ""
  (multiple-value-bind
    (tag id classes) (parse-initial-tag (first body) :as-list as-list)
         (print "body: " body)
         (print "tag " tag)
         (print "id " id)
         (print "classes " classes)
         ))
