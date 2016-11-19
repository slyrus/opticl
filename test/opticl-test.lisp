
(cl:in-package #:opticl-test)

(in-suite :opticl)

(defun test-image (filename)
  (reduce #'merge-pathnames (list filename "test/images/")
          :from-end t
          :initial-value (asdf:component-pathname
                          (asdf:find-system "opticl"))))

(defun output-image (filename)
  (reduce #'merge-pathnames (list filename "test/output/")
          :from-end t
          :initial-value (asdf:component-pathname
                          (asdf:find-system "opticl"))))

(ensure-directories-exist (output-image ""))

(test tiff-read-8-bit-gray-no-compression-jpeg-out
  (let* ((file (test-image "truck-gray-none.tiff"))
         (img (read-tiff-file file)))
    (let ((out (output-image "truck-gray.jpeg")))
      (is (equal out
                 (write-jpeg-file out img))))))

(test tiff-read-8-bit-rbg-no-compression-jpeg-out
  (let* ((file (test-image "truck-rgb-none.tiff"))
         (img (read-tiff-file file)))
    (let ((out (output-image "truck-rgb.jpeg")))
      (is (equal out
                 (write-jpeg-file out img))))))

(test tiff-read-8-bit-gray-no-compression
  (let* ((file (test-image "truck-gray-none.tiff"))
         (img (read-tiff-file file)))
    (let ((out (output-image "truck-gray-from-none.tiff")))
      (is (equal out
                 (write-jpeg-file out img))))))

(test tiff-read-8-bit-gray-lzw-compression
  (let* ((file (test-image "truck-gray-lzw.tiff"))
         (img (read-tiff-file file)))
    (let ((out (output-image "truck-gray-from-lzw.tiff")))
      (is (equal out
                 (write-jpeg-file out img))))))

(test tiff-read-8-bit-gray-packbits-compression
  (let* ((file (test-image "truck-gray-packbits.tiff"))
         (img (read-tiff-file file)))
    (let ((out (output-image "truck-gray-from-packbits.tiff")))
      (is (equal out
                 (write-jpeg-file out img))))))

(test tiff-read-8-bit-gray-deflate-compression
  (let* ((file (test-image "truck-gray-deflate.tiff"))
         (img (read-tiff-file file)))
    (let ((out (output-image "truck-gray-from-deflate.tiff")))
      (is (equal out
                 (write-jpeg-file out img))))))

(test tiff-read-8-bit-gray-jpeg-compression
  (let* ((file (test-image "truck-gray-jpeg.tiff"))
         (img (read-tiff-file file)))
    (let ((out (output-image "truck-gray-from-jpeg.tiff")))
      (is (equal out
                 (write-jpeg-file out img))))))

