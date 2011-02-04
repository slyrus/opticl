
(asdf:defsystem :opticl-test
  :name "opticl-test"
  :author "Cyrus Harmon <ch-lisp@bobobeach.com>"
  :licence "BSD"
  :serial t
  :default-component-class cl-source-file
  :depends-on (opticl)
  :components
  ((:module :test
            :components ((:file "package")
                         (:file "opticl-test")))))
