(in-package #:spasm)


(defun concat (&rest strings)
  "
  A convenience function for concatenating strings.
  "
  (apply #'concatenate 'string strings))

(defun car-pair (plist)
  "
  Get the first pair from a plist.
  "
  ; TODO this should probably be changed to return a naked nil instead of nil
  ; in the case of an empty result
  (cons (car plist) (cadr plist)))

(defun cdr-pairs (plist)
  "
  Get everything after the first pair in a plist.
  "
  (cddr plist))

(defun get-pairs-experimental (plist)
  "
  Create an alist from key/values in a plist.
  "
  ; TODO this implementation is currently broken; creates nested lists instead
  ; of a list of pairs
  (if plist
    (list (car-pair plist) (get-pairs (cdr-pairs plist)))))

(defun get-pairs (plist)
  "
  Returns a list of plists (pairs) containing the same keys and data as the
  property list PLIST in the same order.

  Note that this code is the same as the alexandria:plist-alist, changed to use
  a (list) rather than a (cons). See the following url:

    http://common-lisp.net/~trittweiler/darcs/alexandria/lists.lisp
  "
  (let (list-of-pairs)
    (do ((tail plist (cddr tail)))
        ((endp tail) (nreverse list-of-pairs))
      (push (list (car tail) (cadr tail)) list-of-pairs))))
