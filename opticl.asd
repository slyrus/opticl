
(asdf:defsystem :opticl
  :name "opticl"
  :author "Cyrus Harmon <ch-lisp@bobobeach.com>"
  :licence "BSD"
  :serial t
  :default-component-class cl-source-file
  :components
  ((:static-file "README")
   (:file "package")
   (:file "opticl")))
