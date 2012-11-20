(in-package #:spasm-tests)

(use-package 'spasm)

(deftestsuite container-tags-test-case (spasm-test-suite) ())

(addtest (container-tags-test-case)
  test-container-tags
  (ensure-different nil (member ':a *container-tags*))
  (ensure-different nil (member ':div *container-tags*))
  (ensure-different nil (member ':option *container-tags*))
  (ensure-different nil (member ':ul *container-tags*)))

(addtest (container-tags-test-case)
  test-non-container-tags
  (ensure-same nil (member ':br *container-tags*))
  (ensure-same nil (member ':hl *container-tags*))
  (ensure-same nil (member ':img *container-tags*)))

(addtest (container-tags-test-case)
  test-container-tag-true
  (ensure-same t (container-tag? ':a))
  (ensure-same t (container-tag? ':div))
  (ensure-same t (container-tag? ':option))
  (ensure-same t (container-tag? ':ul)))

(addtest (container-tags-test-case)
  test-container-tag-false
  (ensure-same nil (container-tag? ':br))
  (ensure-same nil (container-tag? ':hl))
  (ensure-same nil (container-tag? ':img)))
