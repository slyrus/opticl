
(in-package #:opticl-test)

(defun test-gamma ()
  (declare (optimize (debug 3)))
  (let ((img (test-circles)))
    (write-png-file "test/output/gamma-orig.png" img)
    (let ((img2 (opticl::apply-gamma img 0.5)))
      (write-png-file "test/output/gamma-0.5.png" img2))
    (let ((img3 (opticl::apply-gamma img 2)))
      (write-png-file "test/output/gamma-2.png" img3))))

