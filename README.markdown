# CL-POV
![logo](examples/logo.png)

CL-POV is CommonLisp binding of the *Persistence of Vision Raytracer*, or *POV-Ray*.

## Usage

```lisp
;; sample.lisp
(ql:quickload :cl-pov :silent t)
(pov:ray t
  (:include "colors.inc")
  (:camera
   (:location <0 0 -3>)
   (:look_at <0 0 0>))
  (:light_source
   <100 100 -100>
   (:color (:rgb <1 1 1>)))
  (:object
   (:sphere <0 0 0> 1)
   (:texture
    (:pigment (:rgb <1 0 0>)))))
```
	
```shellscript
$ clisp sample.lisp > sample.pov
$ povray sample.pov
```

or

```shellscript
(for Roswell user)
$ render sample.lisp
```

See also
* [POV-Ray Official Documentation](http://www.povray.org/documentation/)
* [POV-Ray Unofficial Documentation(ja)](http://www.arch.oita-u.ac.jp/povjp/)

## Installation

```shellscript
$ mkdir ~/common-lisp && cd ~/common-lisp
$ git clone git://github.com/asciian/cl-pov
```

## Requirements

cl-pov supports POV-Ray 3.5
* [POV-Ray](http://www.povray.org)

## Author

* asciian (asciian@outlook.jp)

## Copyright

Copyright (c) 2015 asciian (asciian@outlook.jp)

## License

Licensed under the GPLv3 License.
