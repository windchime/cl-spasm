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


(defun merge-plists (p1 p2)
  "
  Merge two plists.

  Keys in the second plist will take precedence over the first.

  Note that this code was adapted from an answer on Stackoverflow provided by
  the esteemed Rainer Joswig:

    http://stackoverflow.com/questions/3398602/easy-way-to-merge-plists
  "
  (loop with notfound = '#:notfound
        for (key datum) on p1 by #'cddr
        when (eq (getf p2 key notfound) notfound)
        do (progn
             (push datum p2)
             (push key p2)))
  p2)


(defun ends-in-pair? (body)
  "
  Check if the last two components of an HTML S-expression constitute a
  key/data pair.

  Note that for body of length 2, the first component will be the tag (in some
  form), not an attr pair, so the second (last element) in that case will be
  the content.
  "
  (cond ((= (length body) 2) nil)
        (t (keywordp (cadr (reverse body))))))
