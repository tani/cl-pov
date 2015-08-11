(in-package :cl-user)
(defpackage cl-pov-test
  (:use :cl
        :cl-pov
        :prove))
(in-package :cl-pov-test)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-pov)' in your Lisp.

(plan 3)

(defmacro test (arg)
  `(is (pov:ray nil (getf ,arg :case))
       (getf ,arg :result) #'string=))

(test
 '(:case
   (:global_settings
    (:adc_bailout 1/255)
    (:ambient_light <1 1 1>)
    (:hf_gray_16 :off)
    (:irid_wavelength <0.25 0.18 0.14>)
    (:charset :ascii)
    (:max_intersections 64)
    (:max_trace_level 5)
    (:number_of_waves 10)
    (:noise_generator 2))
   :result
"global_settings {
adc_bailout 1/255
ambient_light <1,1,1>
hf_gray_16 off
irid_wavelength <0.25,0.18,0.14>
charset ascii
max_intersections 64
max_trace_level 5
number_of_waves 10
noise_generator 2
}"))

(test 
 '(:case
   (:camera
    (:perspective)
    (:location <0 0 0>)
    (:direction <0 0 1>)
    (:right <1.33 0 0>)
    (:sky <0 1 0>)
    (:up <0 1 0>)
    (:look_at <0 0 1>))
   :result
"camera {
perspective
location <0,0,0>
direction <0,0,1>
right <1.33,0,0>
sky <0,1,0>
up <0,1,0>
look_at <0,0,1>
}"))

(test
 '(:case
   (:light_group
    (:light_source
     <5 -3 5>
     (:color 0.7)
     (:parallel)
     (:point_at <5 -1 0>))
    (:sphere
     <-1.5 0 1> 1
     (:pigment
      (:color (:rgb <0.7 0.5 0.5>)))))
   :result
"light_group {
light_source {
<5,-3,5>
color 0.7
parallel
point_at <5,-1,0>
}
sphere {
<-1.5,0,1>,1
pigment {
color rgb <0.7,0.5,0.5>
}
}
}"))
(finalize)
