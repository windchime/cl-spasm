(ql:quickload 'asdf)
(ql:quickload 'cl-ppcre)
(ql:quickload 'lift)
(ql:quickload 'html-entities)

(defpackage #:spasm-system
  (:use #:cl #:asdf #:cl-ppcre #:html-entities))

(in-package #:spasm-system)

(defsystem spasm
  :serial t
  :description "A Common Lisp port of Hiccup, Clojure's HTML-building library"
  :author "Duncan McGreggor <duncan@cogitat.io>"
  :license "BSD"
  :version "0.1"
  :components (
    (:module "src"
     :components (
       (:file "package")
       (:file "core")
       (:file "macro")
       (:file "element")
       (:file "form")
       (:file "middleware")
       (:file "page")
       (:file "util")))))

(defsystem spasm-tests
  :depends-on (#:spasm)
  :components (
    (:module "tests"
     :components (
       (:file "core")
       (:file "element")
       (:file "util")))))
