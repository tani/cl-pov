#|
  This file is a part of cl-pov project.
  Copyright (c) 2015 Masaya TANIGUCHI (ta2gch@gmail.com)
|#

#|
  Author: Masaya TANIGUCHI (ta2gch@gmail.com)
|#

(in-package :cl-user)
(defpackage cl-pov-asd
  (:use :cl :asdf))
(in-package :cl-pov-asd)

(defsystem cl-pov
  :version "0.1"
  :author "Masaya TANIGUCHI"
  :license "GPLv3"
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "cl-pov"))))
  :description ""
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op cl-pov-test))))
