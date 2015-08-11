(in-package :cl-user)
(defpackage cl-pov-test
  (:use :cl
        :cl-pov
        :prove))
(in-package :cl-pov-test)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-pov)' in your Lisp.

(plan 1)
(is 1 1)
(finalize)
