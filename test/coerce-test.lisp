
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

(test coerce-rgba-image-to-8-bit-rgb-image
  (let* ((file (test-image "truck-rgba.tiff"))
         (img (read-tiff-file file)))
    (let ((output-img (coerce-image img '8-bit-rgb-image)))
      (is (typep output-img '8-bit-rgb-image)))))

(test coerce-rgba-image-to-8-bit-gray-image
  (let* ((file (test-image "truck-rgba.tiff"))
         (img (read-tiff-file file)))
    (let ((output-img (coerce-image img '8-bit-gray-image)))
      (is (typep output-img '8-bit-gray-image)))))

(test coerce-rgba-image-to-8-bit-gray-alpha-image
  (let* ((file (test-image "truck-rgba.tiff"))
         (img (read-tiff-file file)))
    (let ((output-img (coerce-image img '8-bit-gray-alpha-image)))
      (is (typep output-img '8-bit-gray-alpha-image)))))
