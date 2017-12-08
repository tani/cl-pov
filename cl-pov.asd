#|
  This file is a part of cl-pov project.
  Copyright (c) 2015 asciian (asciian@outlook.jp.com)
|#

#|
  Author: asciian (asciian@outlook.jp.com)
|#

(in-package :cl-user)
(defpackage cl-pov-asd
  (:use :cl :asdf))
(in-package :cl-pov-asd)

(defsystem cl-pov
  :version "0.1"
  :author "asciian"
  :license "GPLv3"
  :depends-on (:cl-ppcre)
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
