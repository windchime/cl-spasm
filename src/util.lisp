(in-package #:spasm)

(defvar *regex-id-class*
  "([^ .#]+)(?:#([^ .#]+))?(?:\.([^ #]+))?")

(defun parse-initial-tag (tag)
  "Given an XHTML tag, parse it for CSS id and/or classes.

  Returns a list of (tag-name css-id and css-classes) where the CSS classes are
  dot-separated.
  "
  (let ((tag-name nil)
        (id nil)
        (classes nil))
    (cl-ppcre:do-register-groups (parsed-tag-name parsed-id parsed-classes)
      (*regex-id-class* tag)
      (setf tag-name parsed-tag-name
            id parsed-id
            classes (substitute #\Space #\. parsed-classes)))
    (list tag-name id classes)))
