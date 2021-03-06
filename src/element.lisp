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


(defun normalize-element (body)
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
    (tag id classes) (parse-initial-tag (car body))
    (let ((tag-attrs ())
          (map-attrs (cdr body))
          (tag-content nil))
      ;; if the body ends in a pair, the last element is not content
      (cond ((ends-in-pair? body) (setf tag-content nil))
            ;; if it doesn't end in a pair, then the last element is content
            ;; and needs to be taken out of the map-attrs
            (t (setf map-attrs (butlast map-attrs))
               ;; the body may only be a tag or tag+sytnactic sugar, in which
               ;; case there is no content; if it's longer than one element
               ;; long, let's set the tag content to the last element of the
               ;; body
               (cond ((/= (length body) 1)
                      (setf tag-content (car (last body)))))
               ; TODO the nested bit here needs a unit test!
               ;; if the content is a cons and not a string, we need to do some
               ;; more work... time to recurse
               (cond ((consp tag-content)
                      (setf tag-content
                            (multiple-value-list
                              (normalize-element tag-content)))))))
      (cond (id (setf tag-attrs (merge-plists `(:id ,id) tag-attrs))))
      (cond (classes
              (setf tag-attrs (merge-plists `(:class ,classes) tag-attrs))))
      (cond (map-attrs
              (values tag (merge-plists tag-attrs map-attrs) tag-content))
            (t (values tag tag-attrs tag-content))))))


(defun make-element (tag-name &key attrs content)
  "
  Given a tag string (e.g., 'div', 'br', etc.) and optional plist or attributes
  and/or tag content, create an HTML string.
  "
  (cond ((or content (container-tag? tag-name))
         (concat (make-tag tag-name :attrs attrs)
                 (cond (content content))
                 (make-tag tag-name :closing t)))
        (t (make-tag tag-name :attrs attrs :empty t))))


(defun render-element (body)
  "
  Given an HTML S-expression, render it in actual HTML.
  "
  (multiple-value-bind
    (tag attrs content) (normalize-element body)
    ; if the content is a cons and not a string, we need to call make element
    ; on it (it's already been recursively processed by normalize-element)
    (cond ((consp content)
           (setf content (make-element
                           (car content)
                           :attrs (cadr content)
                           :content (caddr content)))))
    (make-element tag :attrs attrs :content content)))
