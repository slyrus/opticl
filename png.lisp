;;; Copyright (c) 2011 Cyrus Harmon, All rights reserved.
;;; See COPYRIGHT file for details.

(in-package :opticl)

(defun read-png-stream (stream)
  (let ((png (png-read:read-png-datastream stream)))
    (with-accessors
          ((colour-type png-read:colour-type)
           (bit-depth png-read:bit-depth)
           (width png-read:width)
           (height png-read:height)
           (image-data png-read:image-data))
        png
      (cond ((and (eq colour-type :truecolor)
                  (eql bit-depth 8))
             (let ((img (make-8-bit-rgb-image height width)))
               (declare (type 8-bit-rgb-image img))
               (loop for i below height
                  do 
                    (loop for j below width
                       do 
                         (setf (pixel img i j)
                               (values (aref image-data j i 0)
                                       (aref image-data j i 1)
                                       (aref image-data j i 2)))))
               img))
            
            ((and (eq colour-type :truecolor-alpha)
                  (eql bit-depth 8))
             (let ((img (make-8-bit-rgba-image height width)))
               (declare (type 8-bit-rgba-image img))
               (loop for i below height
                  do 
                    (loop for j below width
                       do 
                         (setf (pixel img i j)
                               (values (aref image-data j i 0)
                                       (aref image-data j i 1)
                                       (aref image-data j i 2)
                                       (aref image-data j i 3)))))
               img))

            ;;; the README says the colors are indexed -- but then on
            ;;; the next line says they're decoded. looks like decoded
            ;;; wins.
            ((and (eq colour-type :indexed-colour)
                  (eql bit-depth 8))
             (let ((img (make-8-bit-rgb-image height width)))
               (declare (type 8-bit-rgb-image img))
               (loop for i below height
                  do 
                    (loop for j below width
                       do 
                         (setf (pixel img i j)
                               (values (aref image-data j i 0)
                                       (aref image-data j i 1)
                                       (aref image-data j i 2)))))
               img))

            ((and (eq colour-type :greyscale)
                  (eql bit-depth 8))
             (let ((img (make-8-bit-gray-image height width)))
               (declare (type 8-bit-gray-image img))
               (loop for i below height
                  do 
                    (loop for j below width
                       do 
                         (setf (pixel img i j)
                               (aref image-data j i))))
               img))

            (t
             (error "unable to read PNG image -- fix read-png-stream!"))))))

(defun read-png-file (pathname)
  (with-open-file (stream pathname :direction :input :element-type '(unsigned-byte 8))
    (read-png-stream stream)))


(defun write-png-stream (stream image)
  (typecase image
    (8-bit-rgb-image
     (with-image-bounds (ymax xmax)
         image
       (let ((displaced
              (make-array (* ymax xmax 3)
                          :element-type '(unsigned-byte 8)
                          :initial-contents
                          (make-array (* ymax xmax 3)
                                      :element-type '(unsigned-byte 8)
                                      :displaced-to (transpose-image image)))))
         (zpng:write-png-stream 
          (make-instance 'zpng:png
                         :color-type :truecolor
                         :height xmax
                         :width ymax
                         :bpp 8
                         :image-data displaced)
          stream))))
    (8-bit-rgba-image
     (with-image-bounds (ymax xmax)
         image
       (let ((displaced
              (make-array (* ymax xmax 3)
                          :element-type '(unsigned-byte 8)
                          :initial-contents
                          (make-array (* ymax xmax 4)
                                      :element-type '(unsigned-byte 8)
                                      :displaced-to (transpose-image image)))))
         (zpng:write-png-stream 
          (make-instance 'zpng:png
                         :color-type :truecolor-alpha
                         :height xmax
                         :width ymax
                         :bpp 8
                         :image-data displaced)
          stream))))
    (8-bit-gray-image
     (with-image-bounds (ymax xmax)
         image
       (let ((displaced
              (make-array (* ymax xmax)
                          :element-type '(unsigned-byte 8)
                          :initial-contents
                          (make-array (* ymax xmax)
                                      :element-type '(unsigned-byte 8)
                                      :displaced-to (transpose-image image)))))
         (zpng:write-png-stream 
          (make-instance 'zpng:png
                         :color-type :grayscale
                         :height xmax
                         :width ymax
                         :bpp 8
                         :image-data displaced)
          stream))))
    (t (error "No PNG writing support for this image type."))))

(defun write-png-file (pathname image)
  (with-open-file (stream pathname :direction :output
                          :if-exists :supersede
                          :element-type '(unsigned-byte 8))
    (write-png-stream stream image)))
