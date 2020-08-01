
(cl:in-package #:opticl-test)

(in-suite :opticl)

(test coerce-grayscale-alpha-image-to-rgb-image
  (let* ((file (test-image "truck-gray-alpha.png"))
         (img (read-png-file file)))
    (let ((output-img (coerce-image img 'rgb-image)))
      (is (typep output-img 'rgb-image)))))

(test coerce-grayscale-alpha-image-to-8-bit-rgb-image
  (let* ((file (test-image "truck-gray-alpha.png"))
         (img (read-png-file file)))
    (let ((output-img (coerce-image img '8-bit-rgb-image)))
      (is (typep output-img '8-bit-rgb-image)))))
