
(asdf:defsystem :opticl
  :name "opticl"
  :author "Cyrus Harmon <ch-lisp@bobobeach.com>"
  :licence "BSD"
  :serial t
  :default-component-class cl-source-file
  :depends-on (alexandria retrospectiff zpng png-read cl-jpeg skippy)
  :components
  ((:static-file "README.md")
   (:static-file "COPYRIGHT")
   (:file "package")
   (:file "opticl")
   (:file "coerce")
   (:file "colors")
   (:file "imageops")
   (:file "invert")
   (:file "transform")
   (:file "convolve")
   (:file "morphology")
   (:file "gamma")
   (:file "shapes")
   (:file "tiff")
   (:file "jpeg")
   (:file "png")
   (:file "pnm")
   (:file "gif")
   (:file "io")
   (:file "cluster")))
