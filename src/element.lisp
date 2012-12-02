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

(defun parse-initial-tag (tag &key (as-list nil))
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
    (cond (as-list (list tag-name id classes))
          (t (values tag-name id classes)))))

(defun make-element (tag-name &key attrs content)
  ""
  (cond (content ())
        ((container-tag? tag-name) (make-tag tag-name :attrs attrs)
                                   (make-tag tag-name :closing t))
        (t (make-tag tag-name :attrs attrs :empty t))))

(defun normalize-element (body &key as-list)
  "
  Bring an HTML form into a standard representation.

  An HTML form could be any of the following (as well as many more):
    * (:p)
    * (:p 'content')
    * (:p#myid :class 'style 1' 'content')
    * (:p#myid.style1.style2 'content')

  This function aims to unify all the different representations into a single
  form through the following:
    * separating the tag from the body
    * expanding all CSS-type syntactic sugar to plists
    * putting all attrs in a single data structure
    * identifying the content, if any, and separating this out from the body
  "
  (multiple-value-bind
    (tag id classes) (parse-initial-tag (car body) :as-list as-list)
    (let ((tag-attrs ())
          (map-attrs (cdr body))
          (tag-content (last body)))
      (cond ((keywordp (cadr (reverse body))) (setf tag-content nil))
            (t (setf map-attrs (butlast map-attrs))))
      (cond (id (append (:id id) tag-attrs))
            (classes (append (:class classes) tag-attrs)))
      (cond (map-attrs
              (list tag (merge-plists tag-attrs map-attrs) tag-content))
            (t (list tag tag-attrs tag-content))))))
