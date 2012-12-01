(in-package #:spasm)


;(defgeneric compile-element ()
;  (:documentation
;    "Returns an unevaluated form that will render the supplied list as an HTML
;    element."))

;(defmethod compile-element
;  (())
;  )


;(defun compile-list (content)
  ; iterate through each expression in the content
  ; is it a list? then compile-element
  ; is it a literal? then return the expression
  ; is it a list? then compile-form
  ; else render the expression as html
;  )

;(defun collapse-strings (expr)
;  )

;(defun collapse-list (l)
;  (cond ((atom l) (cons l nil))
;        ((null (cdr l))
;         (cond ((atom (car l)) l)
;             (t (collapse-list (car l)))))
;        (t (append (collapse-list (car l))
;                   (collapse-list (cdr l))))))

(defmacro html (body)
  `(list ,@body))
  ;(collapse-strings `(str @(compile-list content))))
