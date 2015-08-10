(ql:quickload :cl-pov :silent t)
(pov:ray t
  (:include #?"colors.inc")
  (:camera
   (:location <0 0 -3>)
   (:look_at <0 0 0>))
  (:light_source
   <100 100 -100>
   (:color "White"))
  (:object
   (:sphere <0 0 0> 1)
   (:texture
    (:pigment "Red"))))
