(in-package #:spasm)

(defvar *regex-id-class*
  "([^ .#]+)(?:#([^ .#]+))?(?:\.([^ #]+))?")

(defun parse-initial-tag (tag)
  (cl-ppcre:do-register-groups (tag-name id classes)
    (*regex-id-class* tag)
    (print (list tag-name id classes))))
