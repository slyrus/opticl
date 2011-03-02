
(asdf:defsystem :opticl-test
  :name "opticl-test"
  :author "Cyrus Harmon <ch-lisp@bobobeach.com>"
  :licence "BSD"
  :default-component-class cl-source-file
  :depends-on (opticl)
  :pathname #.(make-pathname :directory '(:relative "test"))
  :components
  ((:file "package")
   (:file "opticl-test")
   (:file "shapes-test")
   (:file "transform-test")
   (:file "gamma-test")
   (:module "images")))
