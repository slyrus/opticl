;;; Copyright (c) 2011 Cyrus Harmon, All rights reserved.
;;; See COPYRIGHT file for details.

(in-package #:opticl-test)

(defun test-image (filename)
  (reduce #'merge-pathnames (list filename "images/")
          :from-end t
          :initial-value (asdf:component-pathname
                          (reduce #'asdf:find-component
                                  '("opticl-test" "test")
                                  :initial-value nil))))

(defun output-image (filename)
  (reduce #'merge-pathnames (list filename "output/")
          :from-end t
          :initial-value (asdf:component-pathname
                          (reduce #'asdf:find-component
                                  '("opticl-test" "test")
                                  :initial-value nil))))

(defun tiff-image (filename)
  (reduce #'merge-pathnames (list filename "images/")
          :from-end t
          :initial-value (asdf:component-pathname
                          (asdf:find-system "retrospectiff"))))

(ensure-directories-exist (output-image ""))

(defun test-tiff-read-8-bit-gray ()
  (let* ((file (test-image "truck-gray.tiff"))
         (img (read-tiff-file file)))
    (let ((out (output-image "truck-gray.jpeg")))
      (write-jpeg-file out img))))

(defun test-tiff-read-8-bit-rgb ()
  (let* ((file (test-image "truck.tiff"))
         (img (read-tiff-file file)))
    (let ((out (output-image "truck.jpeg")))
      (write-jpeg-file out img))))

(defun test-tiff-read-16-bit-rgb ()
  (let* ((file (test-image "truck-16.tiff"))
         (img (read-tiff-file file)))
    (declare (ignore img))
    #+nil (let ((out (output-image "truck-16.jpeg")))
            (write-jpeg-file out img))))

(defun test-jpeg-read-8-bit-rgb ()
  (let* ((file (test-image "truck.jpeg"))
         (img (read-jpeg-file file)))
    (let ((out (output-image "truck.jpeg")))
      (write-jpeg-file out img))))

(defun test-jpeg-read-8-bit-gray ()
  (let* ((file (test-image "truck-gray.jpeg"))
         (img (read-jpeg-file file)))
    (let ((out (output-image "truck-gray.jpeg")))
      (write-jpeg-file out img))))

(defun test-png-read-8-bit-rgb ()
  (let* ((file (test-image "truck.png"))
         (img (read-png-file file)))
    (let ((out (output-image "truck.jpeg")))
      (write-jpeg-file out img))))


(defun test-snow ()
  (let* ((file (tiff-image "snow.tiff"))
         (img (read-tiff-file file)))
    (let ((out (output-image "snow-rgba.jpeg")))
      (write-jpeg-file out img))))

(defun test-snow-lzw ()
  (let* ((file (tiff-image "snow-lzw.tiff"))
         (img (read-tiff-file file)))
    (let ((out (output-image "snow-lzw.jpeg")))
      (write-jpeg-file out img))))

(defun test-snow-rgb ()
  (let* ((file (tiff-image "snow-rgb.tiff"))
         (img (read-tiff-file file)))
    (let ((out (output-image "snow-rgb.jpeg")))
      (write-jpeg-file out img))))

(defun test-snow-packbits ()
  (let* ((file (tiff-image "snow-packbits.tiff"))
         (img (read-tiff-file file)))
    (let ((out (output-image "snow-packbits.jpeg")))
      (write-jpeg-file out img))))

(defun test-read-pgm (&key (binary t))
  (let* ((file (test-image "knight0.pgm"))
         (img (opticl:read-pgm-file file)))
    (let ((out (output-image "knight0.pgm")))
      (opticl:write-pgm-file out img :binary binary))))

(defun test-pbm-read-ascii ()
  (let* ((file (test-image "goat-ascii.pbm"))
         (img (opticl:read-pnm-file file)))
    img))

(defun test-pbm-write-ascii ()
  (let* ((file (test-image "goat-ascii.pbm"))
         (img (opticl:read-pbm-file file)))
    (let ((out (output-image "goat-ascii.pbm")))
      (opticl:write-pbm-file out img :binary nil))))

(defun test-pbm-read-binary-write-ascii ()
  (let* ((file (test-image "goat.pbm"))
         (img (opticl:read-pbm-file file)))
    (let ((out (output-image "goat-ascii.pbm")))
      (opticl:write-pbm-file out img :binary nil))))

(defun test-pbm-write-binary ()
  (let* ((file (test-image "goat-ascii.pbm"))
         (img (opticl:read-pbm-file file)))
    (let ((out (output-image "goat-binary.pbm")))
      (opticl:write-pbm-file out img))))

(defun test-ppm-write-ascii ()
  (let* ((file (test-image "truck.png"))
         (img (opticl:read-png-file file)))
    (let ((out (output-image "truck-ascii.ppm")))
      (opticl:write-ppm-file out img :binary nil))))

(defun test-ppm-write-binary ()
  (let* ((file (test-image "truck.png"))
         (img (opticl:read-png-file file)))
    (let ((out (output-image "truck-binary.ppm")))
      (opticl:write-ppm-file out img))))

