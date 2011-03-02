
(asdf:defsystem :opticl
  :name "opticl"
  :author "Cyrus Harmon <ch-lisp@bobobeach.com>"
  :licence "BSD"
  :serial t
  :default-component-class cl-source-file
  :depends-on (retrospectiff zpng png-read #-abcl cl-jpeg)
  :components
  ((:static-file "README.md")
   (:static-file "COPYRIGHT")
   (:file "package")
   (:file "opticl")
   (:file "imageops")
   (:file "invert")
   (:file "transform")
   (:file "convolve")
   (:file "gamma")
   (:file "shapes")
   (:file "tiff")
   #-abcl (:file "jpeg")
   #+abcl (:file "opticl-abcl")
   (:file "png")
   (:file "pnm")))
