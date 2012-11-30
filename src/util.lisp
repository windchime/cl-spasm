(in-package #:spasm)

(defun concat (&rest strings)
  "A convenience function for concatenating strings."
  (apply #'concatenate 'string strings))
