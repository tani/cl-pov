#|
Copyright (c) 2015 Masaya TANIGUCHI (ta2gch@gmail.com)

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
|#

(cl:in-package #:cl)
(defpackage #:cl-pov
  (:use #:cl)
  (:nicknames #:pov)
  (:export ray *POV-Ray-version*))
(in-package #:pov)

(defvar *POV-Ray-version* 3.5)
(set-macro-character #\> (get-macro-character #\)))
(set-macro-character
 #\<
 #'(lambda (stream char1)
     (declare (ignore char1))
     (let ((ls (read-delimited-list #\> stream t)))
       `(format nil "<~@{~10r~^,~}>" ,@ls))))

(set-dispatch-macro-character
 #\# #\?
 #'(lambda (stream char1 char2)
     (declare (ignore char1 char2))
     (write-to-string (read stream t nil t))))

(defun parse (sexp)
  "dummy function"
  (identity sexp))

(defun on (sexp)
  (format nil "~(~a~)" sexp))
(defparameter on
  '(:x :y :z
    ;;global_settings
    :ascii :utf8 :sys :on :off
    ;;camera_type
    :perspective :orthographic
    :fisheye :ultra_wide_angle
    :omnimax :panoramic :cylinder :spherical
    ;;light_source
    :spotlight :cylinder :parallel
    :shadowless :on :off
    ;;light_source area_light
    :jitter :circular :orient
    ;;light_group global_lights
    :on :off
    ;;global_settings radiosity
    :on :off
    ;;objects mesh hierarchy
    :on :off
    ;;objects sor
    :open :sturm
    ;;objects lathe
    :linear_spline :quadratic_spline
    :cubic_spline :bezier_spline :sturm
    ;;objects prism
    :linear_sweep :conic_sweep
    :linear_spline :quadratic_spline
    :cubic_spline :bezier_spline
    :open :sturm
    ;;objects sphere_sweep
    :linear_spline :b_spline :cubic_spline
    ;;objects height_field
    :smooth :on :off
    ;;objects blob
    :on :off :sturm
    ;;objects julia_fractal
    :quaternion :hypercomplex
    :cube :exp :reciprocal :sin :asin :sinh
    :asinh :cos :acos :cosh :acosh :tan :atan :tanh
    :atanh :ln ; not support :pwr( X_Val, Y_Val )
    ;;objects cubic & quatric & poly
    :sturm
    ;;objects Options
    :clipped_by :bounded_by :hollow :no_shadow
    :no_image :no_reflection :double_illuminate
    :inverse :sturm
    ;;finish reflection
    :on :off
    ;;texture cutaway_textures
    :cutaway_textures
    ;;texture pattern crackle 
    :solid
    ;;texture option wave
    :ramp_wave :triangle_wave :sine_wave
    :scallop_wave :cubic_wave :poly_wave
    ;;texture option
    :once
    ))

(defun area_light (sexp)
  (format
   nil "~(~a~) ~{~a~^, ~}"
   (first sexp)
   (parse (rest sexp))))
(defparameter area_light
  '(;;light_source area_light
    :area_light
    ;;media
    :samples
    ;;photons
    :gather :media :expand_thresholds :radius
    ;;objects mesh triangle
    :uv_vectors
    ;;objects mesh smooth_triangle
    :uv_vectors
    ;;objects mesh2
    :vertex_vectors :normal_vectors :uv_vectors
    :texture_list :face_indices :normal_indices :uv_indices
    ;;objects julia_fractal
    :slice
    ;;objects text
    :ttf
    ;;texture pattern
    :brick :checker :hexagon
    :gradient :mandel :julia
    ))

(defun scattering (sexp)
  (format
   nil "~(~a~) {~%~2{~a~^,~}~%~{~a~%~}}"
   (first sexp)
   (parse (subseq sexp 1 3))
   (parse (subseq sexp 3))))

(defparameter scattering
  '(;;media scattering
    :scattering
    ;;object
    :box :sphere :torus :plane :poly
    ))

(defun cylinder (sexp)
  (format
   nil "~(~a~) {~%~3{~a~^,~}~%~{~a~%~}}"
   (first sexp)
   (parse (subseq sexp 1 4))
   (parse (subseq sexp 4))))
(defparameter cylinder
  '(;;objects
    :cylinder :triangle
    ))

(defun cone (sexp)
  (format
   nil "~(~a~) {~%~4{~a~^,~}~%~{~a~%~}}"
   (first sexp)
   (parse (subseq sexp 1 5))
   (parse (subseq sexp 5))))
(defparameter cone
  '(;;objects
    :cone :disc :quadric
    ))

(defun smooth_triangle (sexp)
  (format
   nil "~(~a~) {~%~6{~a,~a~^,~%~}~%~{~a~%~}}"
   (first sexp)
   (parse (subseq sexp 1 7))
   (parse (subseq sexp 7))))
(defparameter smooth_triangle
  '(;objects
    :smooth_triangle 
    ))

(defun polygon (sexp)
  (let ((n (second sexp)))
    (format
     nil "~(~a~) {~%~{a~}~%~{~a~^,~}~%~{~a~%~}}"
     (first sexp)
     (parse (list (second sexp)))
     (parse (subseq sexp 2 (+ n 2)))
     (parse (subseq sexp (+ n 2))))))
(defparameter polygon
  '(;;objects
    :polygon :sor
    ))

(defun bicubic_patch (sexp)
  (format
   nil "~(~a~) {~%~4{~a~%~}~16{~a~^,~}~%~{~a~%~}}"
   (first sexp)
   (parse (subseq sexp 1 5))
   (parse (subseq sexp 5 21))
   (parse (subseq sexp 21))))
(defparameter bicubic_patch
  '(;;objects
    :bicubic_patch
    ))

(defun sphere_sweep (sexp)
  (format
   nil "~(~a~) {~%~{~a~}~%~{~a~},~%~{~a,~a~%~}~{~a~%~}}"
   (first sexp)
   (parse (list (second sexp)))
   (parse (list (third sexp)))
   (parse (subseq sexp 3 (+ (* 2 (third sexp)) 3)))
   (parse (subseq sexp (+ (* 2 (third sexp)) 3)))))
(defparameter sphere_sweep
  '(;objects
    :sphere_sweep
    ))

(defun lathe (sexp)
  (format
   nil "~(~a~) {~%~{~a~}~%~{~a~},~%~{~a~^,~}~%~{~a~%~}}"
   (first sexp)
   (parse (list (second sexp)))
   (parse (list (third sexp)))
   (parse (subseq sexp 3 (+ (third sexp) 3)))
   (parse (subseq sexp (+ (third sexp) 3)))))
(defparameter lathe
  '(;objects
    :lathe))

(defun color_map (sexp)
  (format
   nil "~(~a~) {~%~{~a~%~}}"
   (first sexp)
   (mapcar
    (lambda (list)
      (format nil "[~{~a~} ~{~a~^ ~}]"
	      (parse (list (first list)))
	      (parse (rest list))))
    (rest sexp))))

(defparameter color_map
  '(;;texture pigment
    :color_map :pigment_map
    ;;texture normal
    :slope_map :normal_map
    ))

(defun prism (sexp)
  (format
   nil "~(~a~) {~%~{~a ~}~{~a,~}~%~{~a~^,~}~%~{~a~%~}}"
   (first sexp)
   (parse (subseq sexp 1 3))
   (parse (subseq sexp 3 6))
   (parse (subseq sexp 6 (+ (fifth sexp) 5)))
   (parse (subseq sexp (+ (fifth sexp) 5)))))
(defparameter prism
  '(;;objects
    prism))

(defun object (sexp)
  (format
   nil "~(~a~) {~%~{~a~%~}}"
   (first sexp)
   (parse (rest sexp))))
(defparameter object
  '(;;top level keywords
    :object :texture :camera :light_source
    ;;global_settings
    :global_settings :radiosity :photons
    ;;light_source
    :look_like :projected_through
    ;;light_group
    :light_group :light_source :object
    ;;atomospheric effects
    :background :sky_sphere :fog :rainbow
    ;;media
    :media :density
    ;;objects
    :superellipsoid :mesh :mesh2 :height_field
    :blob :julia_fractal :text :cubic :quatric ;;not support isosurface
    ;;objects CSG
    :merge :union :difference :intersection :clipped_by :bounded_by
    ;;objects options
    :material
    ;;texture ;;not support color_reflection_min (:reflection)
    :pigment :image_map :normal :bump_map :finish :reflection
    :irid :material_map :texture_list :interior_texture
    :interior
    ;;texture pattern
    :pigment_pattern :image_pattern :object
    ))

(defun rgb (sexp)
  (format
   nil "~(~a~) ~{~a~^ ~}"
   (first sexp)
   (parse (rest sexp))))
(defparameter rgb
  '(;;colors
    :rgb :rgbf :srgb :srgbf
    :red :green :blue :filter :transmit
    ;;transforms
    :rotate :translate :scale :matrix
    ;;global_settings
    :adc_bailout :ambient_light :assumed_gamma
    :hf_gray_16 :irid_wavelength :charset :max_intersections
    :max_trace_level :number_of_waves :noise_generator
    ;;camera
    :location :right :up :direction :sky :angle :look_at
    :aperture :focal_point :blur_samples :confidence :variance
    ;;light_source
    :color :fade_distance :fade_power
    :media_attenuation :media_interaction
    ;;light_source spotlight
    :point_at :radius :falloff :tightness
    ;;light_source cylinder
    :point_at :radius :falloff :tightness
    ;;light_source parallel
    :point_at
    ;;light_source area_light
    :adaptive
    ;;background
    :color
    ;;fog
    :fog_type :distance :color :turbulence :turb_depth
    :omega :lambda :octaves :fog_offset :fog_alt :up
    ;;rainbow
    :direction :angle :width :distance
    :jitter :up :arc_angle :falloff_angle
    ;;media scattring
    :eccentricity :extinction
    ;;media
    :absorption :emission :method :intervals :confidence
    :variance :ratio :aa_level :aa_threshold
    ;;photons
    :spacing :count :jitter :max_trace_level
    :adc_bailout :save_file :load_file :autostop
    ;;radiosity
    :adc_bailout :always_sample :brightness :count :error_bound
    :gray_threshold :low_error_factor :max_sample :media
    :minimum_reuse :nearest_count :normal :pretrace_start
    :pretrace_end :recursion_limit :load_file :save_file
    ;;objects mesh
    :inside :hierarchy
    ;;objects mesh2
    :inside_vector
    ;;objects sphere_sweep
    :tolerance
    ;;objects height_field
    :gif :tga :pot :png :pgm :ppm :jpeg :tiff ; not support :SYS
    :hierarchy :water_level
    ;;objects blob
    :threshold :hierarchy
    ;;objects julia_fractal
    :max_iteration :precision
    ;;objects bicubic_patch
    :type :flatness :u_steps :v_steps
    ;;texture pigment image_map
    :gif :tga :pot :png :pgm :ppm :jpeg :tiff ; not support :SYS
    ;;texture normal bump_map
    :gif :tga :pot :png :pgm :ppm :jpeg :tiff ; not support :SYS
    ;;texture finish hight_light
    :ambient :diffuese :brilliance :crand :phong
    :phong_size :specular :roughness :metallic
    ;;texture finish reflection
    :fresnel :falloff :exponent :metallic
    ;;texture finish irid
    :thickness :turbulence
    ;;texture material_map
    :gif :tga :pot :png :pgm :ppm :jpeg :tiff ; not support :SYS
    ;;texture
    :uv_mapping
    ;;texture interior
    :ior :caustics :dispersion_samples
    :fade_distance :fade_power :fade_color
    ;;texture pattern agate
    :agate_tub
    ;;texture pattern brick
    :brick_size :mortar
    ;;texture pattern crackle
    :crackle :form :metric :offset
    ;;texture pattern
    :magnet :quilted :spiral1 :spiral2
    :agate :average :boxed :bozo
    :bumps :cylindrical :dents
    :granite :leopard :marble :onion
    :planer :radial :ripples :spherical
    :spotted :waves :wood :wrinkles :cells
    :facets 
    ;;texture pattern quilted
    :control0 :control1
    ;;texture pattern facets
    :coords :size
    ;;texture pattern image_pattern
    :gif :tga :pot :png :pgm :ppm :jpeg :tiff ; not support :SYS
    ;;texture options
    :frequency :phase :turbulence :octaves :lambda :omega :wrap
    :map_type :interpolate
    ))

(defun include (sexp)
  (format
   nil "#~(~a~) ~{~a~}"
   (first sexp) (parse (rest sexp))))

(defparameter include
  '(:include))

(defun parse (body)
  (mapcar
   (lambda (sexp)
     (cond ((stringp sexp) sexp)
	   ((numberp sexp) (write-to-string sexp))
	   ((member sexp on) (on sexp))
	   ((symbolp sexp) (eval sexp))
	   ((member (first sexp) on) (on sexp))
	   ((member (first sexp) area_light) (area_light sexp))
	   ((member (first sexp) scattering) (scattering sexp))
	   ((member (first sexp) cylinder) (cylinder sexp))
	   ((member (first sexp) cone) (cone sexp))
	   ((member (first sexp) smooth_triangle) (smooth_triangle sexp))
	   ((member (first sexp) polygon) (polygon sexp))
	   ((member (first sexp) bicubic_patch) (bicubic_patch sexp))
	   ((member (first sexp) sphere_sweep) (sphere_sweep sexp))
	   ((member (first sexp) lathe) (lathe sexp))
	   ((member (first sexp) color_map) (color_map sexp))
	   ((member (first sexp) prism) (prism sexp))
	   ((member (first sexp) object) (object sexp))
	   ((member (first sexp) rgb) (rgb sexp))
	   ((member (first sexp) include) (include sexp))
	   (t (eval sexp))))
   body))

(defmacro ray (stream &body body)
  `(format ,stream "~{~a~%~}" (parse ',body)))

