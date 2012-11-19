(in-package #:spasm)

(defparameter *container-tags*
  '(:a :article :aside :b :body :canvas :dd :div :dl :dt :em :fieldset :footer
       :form :h1 :h2 :h3 :h4 :h5 :h6 :head :header :hgroup :html :i :iframe
       :label :li :nav :ol :option :pre :section :script :span :strong :style
       :table :textarea :title :ul)
  "A list of elements that need an explicit ending tag when rendered.")
