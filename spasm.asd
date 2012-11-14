(asdf:defsystem spasm
  :serial t
  :description "A Common Lisp port of Hiccup, Clojure's HTML-building, vector- and map-based library"
  :author "Duncan McGreggor <duncan@cogitat.io>"
  :license "BSD"
  :version "0.1"
  :components ((:file "package")
               (:file "core")
               (:file "macro")
               (:file "element")
               (:file "form")
               (:file "middleware")
               (:file "page")
               (:file "util")))
