(in-package #:spasm)


(defun concat (&rest strings)
  "A convenience function for concatenating strings."
  (apply #'concatenate 'string strings))

(defun car-pair (plist)
  "Get the first pair from a plist."
  ; TODO this should probably be changed to return a naked nil instead of nil
  ; in the case of an empty result
  (cons (car plist) (cadr plist)))

(defun cdr-pairs (plist)
  "Get everything after the first pair in a plist."
  (cddr plist))

(defun get-pairs (plist)
  "Create an alist from key/values in a plist."
  ; TODO this is currently broken; creates nested lists instead of a list of
  ; pairs
  (if plist
    (list (car-pair plist) (get-pairs (cdr-pairs plist)))))


(defun car-pair (plist)
  "Get the first pair from a plist."
  ; TODO this should probably be changed to return a naked nil instead of nil
  ; in the case of an empty result
  (list (car plist) (cadr plist)))
