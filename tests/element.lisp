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
