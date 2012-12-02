(defpackage #:spasm
  (:use #:cl)
  (:export
    ; core
    #:compile-attr
    #:html
    ; macro
    #:defelem
    #:defhtml
    #:wrap-attrs
    ; element
    #:*container-tags*
    #:*regex-id-class*
    #:coerce-key
    #:container-tag?
    #:html-attr
    #:image
    #:javascript-tag
    #:link-to
    #:mail-to
    #:make-attrs
    #:make-element
    #:make-tag
    #:normalize-element
    #:ordered-list
    #:parse-initial-tag
    #:unordered-list
    ; form
    #:check-box
    #:drop-down
    #:email-field
    #:file-upload
    #:form-to
    #:hidden-field
    #:label
    #:password-field
    #:radio-button
    #:reset-button
    #:select-options
    #:submit-button
    #:text-area
    #:text-field
    #:with-group
    ; middleware
    #:wrap-base-url
    ; page
    #:html4
    #:html5
    #:include-css
    #:include-js
    #:xhtml
    #:xhtml-tag
    #:xml-declaration
    ; util
    #:concat
    #:car-pair
    #:cdr-pairs
    #:get-pairs
    #:merge-plists))
