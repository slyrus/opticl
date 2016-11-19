
(asdf:defsystem :opticl
  :name "opticl"
  :description "A library for representing and processing images"
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
   (:file "cluster")
   (:file "thresholding"))
  :in-order-to ((test-op (test-op :opticl/tests))))


(asdf:defsystem :opticl/tests
  :depends-on (:opticl :fiveam)
  :serial t
  :default-component-class cl-source-file
  :components
  ((:module :test
            :components ((:file "package")
                         (:file "opticl-test")
                         (:file "generate-test-images")
                         (:file "read-test-images"))))
  :perform (test-op (o c)
                    (uiop:symbol-call :fiveam '#:run! :opticl)))


