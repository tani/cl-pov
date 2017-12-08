#|
  This file is a part of cl-pov project.
  Copyright (c) 2015 asciian (asciian@outlook.jp)
|#

(in-package :cl-user)
(defpackage cl-pov-test-asd
  (:use :cl :asdf))
(in-package :cl-pov-test-asd)

(defsystem cl-pov-test
  :author "asciian"
  :license "GPLv3"
  :depends-on (:cl-pov
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "cl-pov"))))
  :description "Test system for cl-pov"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
