(defpackage #:spasm
  (:use #:cl)
  (:export
    ; core
    #:html
    ; macro
    #:defelem
    #:defhtml
    #:wrap-attrs
    ; element
    #:*container-tags*
    #:image
    #:javascript-tag
    #:link-to
    #:mail-to
    #:ordered-list
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
    #:*regex-id-class*
    #:as-str
    #:escape-html
    #:parse-initial-tag
    #:to-str
    #:to-uri
    #:url
    #:url-encode
    #:with-base-url
    #:with-encoding))
