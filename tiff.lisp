;;; Copyright (c) 2011 Cyrus Harmon, All rights reserved.
;;; See COPYRIGHT file for details.

(in-package :opticl)

;;;
;;; Reading TIFF files
(defun read-tiff-stream (stream)
  "reads a TIFF image from a stream and returns either a 32-bit ARGB
image or an 8-bit grayscale image"
  (let ((tiff-image (tiff:read-tiff-stream stream)))
    (with-accessors ((image-length tiff:tiff-image-length)
                     (image-width tiff:tiff-image-width)
                     (samples-per-pixel tiff:tiff-image-samples-per-pixel) 
                     (bits-per-sample tiff:tiff-image-bits-per-sample) 
                     (image-data tiff:tiff-image-data)
                     (color-map tiff:tiff-image-color-map)
                     (min-is-white tiff:tiff-image-min-is-white))
        tiff-image
      (cond

        (color-map  ;; indexed RGB
         (let ((image (make-8-bit-rgb-image image-length image-width)))
           (declare (type 8-bit-rgb-image image))
           (loop for i below image-length
              do 
                (loop for j below image-width
                   do 
                     (let ((pixoff (+ (* i image-width) j)))
                       (setf (pixel* image i j)
                             (mapcar (lambda (x) (ash x -8))
                                     (aref color-map
                                           (aref image-data pixoff)))))))
           image))

        ((and (= samples-per-pixel 1)
              (equalp bits-per-sample 1)) ;; black and white
         (let ((image (make-1-bit-gray-image image-length image-width)))
           (declare (type 1-bit-gray-image image))
           (loop for i below image-length
              do 
                (loop for j below image-width
                   do (setf (pixel image i j)
                            (if min-is-white
                                (ldb (byte 1 (- 7 (mod (+ (* i image-width) j) 8)))
                                     (lognot (aref image-data (ash (+ (* i image-width) j) -3))))
                                (ldb (byte 1 (- 7 (mod (+ (* i image-width) j) 8)))
                                     (aref image-data (ash (+ (* i image-width) j) -3)))))))
           image))

        ((and (= samples-per-pixel 1)
              (equalp bits-per-sample 8)) ;; 8-bit Grayscale
         (let ((image (make-8-bit-gray-image image-length image-width)))
           (declare (type 8-bit-gray-image image))
           (loop for i below image-length
              do 
              (loop for j below image-width
                 do 
                 (let ((pixoff (+ (* i image-width) j)))
                   (setf (pixel image i j)
                         (aref image-data pixoff)))))
           image))

        ((and (= samples-per-pixel 3)
              (equalp bits-per-sample #(8 8 8))) ;; 8-bit RGB
         (let ((image (make-8-bit-rgb-image image-length image-width)))
           (declare (type 8-bit-rgb-image image))
           (loop for i below image-length
              do 
              (loop for j below image-width
                 do 
                 (let ((pixoff (* 3 (+ (* i image-width) j))))
                   (setf (pixel image i j)
                         (values (aref image-data pixoff)
                                 (aref image-data (incf pixoff))
                                 (aref image-data (incf pixoff)))))))
           image))

        ((and (= samples-per-pixel 4)
              (equalp bits-per-sample #(8 8 8 8))) ;; 8-bit RGBA
         (let ((image (make-8-bit-rgba-image image-length image-width)))
           (declare (type 8-bit-rgba-image image))
           (loop for i below image-length
              do 
              (loop for j below image-width
                 do 
                 (let ((pixoff (* 4 (+ (* i image-width) j))))
                   (setf (pixel image i j)
                         (values (aref image-data pixoff)
                                 (aref image-data (incf pixoff))
                                 (aref image-data (incf pixoff))
                                 (aref image-data (incf pixoff)))))))
           image))


        ((and (= samples-per-pixel 3)
              (equalp bits-per-sample #(16 16 16))) ;; 16-bit RGB
         (let ((image (make-16-bit-rgb-image image-length image-width)))
           (declare (type 16-bit-rgb-image image))
           (loop for i below image-length
              do 
              (loop for j below image-width
                 do 
                 (let ((pixoff (* 6 (+ (* i image-width) j))))
                   (setf (pixel image i j)
                         (values (+ (ash (aref image-data pixoff) 8)
                                    (aref image-data (incf pixoff)))
                                 (+ (ash (aref image-data (incf pixoff)) 8)
                                    (aref image-data (incf pixoff)))
                                 (+ (ash (aref image-data (incf pixoff)) 8)
                                    (aref image-data (incf pixoff))))))))
           image))
            
        ((and (= samples-per-pixel 4)
              (equalp bits-per-sample #(16 16 16 16))) ;; 16-bit RGBA
         (let ((image (make-16-bit-rgba-image image-length image-width)))
           (declare (type 16-bit-rgba-image image))
           (loop for i below image-length
              do 
              (loop for j below image-width
                 do 
                 (let ((pixoff (* 8 (+ (* i image-width) j))))
                   (setf (pixel image i j)
                         (values (+ (ash (aref image-data pixoff) 8)
                                    (aref image-data (incf pixoff)))
                                 (+ (ash (aref image-data (incf pixoff)) 8)
                                    (aref image-data (incf pixoff)))
                                 (+ (ash (aref image-data (incf pixoff)) 8)
                                    (aref image-data (incf pixoff)))
                                 (+ (ash (aref image-data (incf pixoff)) 8)
                                    (aref image-data (incf pixoff))))))))))
        (t 
         (error "TIFF decoding error"))))))

(defun read-tiff-file (pathname)
  (with-open-file (stream pathname :direction :input :element-type '(unsigned-byte 8))
    (read-tiff-stream stream)))


;;;
;;; Writing TIFF files
(defun make-tiff-image (image)
  (typecase image
    (8-bit-gray-image
     (destructuring-bind (height width)
         (array-dimensions image)
       (let ((tiff-image (make-instance 'tiff:tiff-image
                                        :width width
                                        :length height
                                        :bits-per-sample 8
                                        :samples-per-pixel 1
                                        :data (make-array (* width height)
                                                          :initial-element 255))))
         (with-accessors ((image-data tiff:tiff-image-data))
             tiff-image
           (let ((pixoff 0))
             (loop for i below height
                do 
                  (loop for j below width
                     do 
                       (setf (aref image-data pixoff) (pixel image i j))
                       (incf pixoff)))))
         tiff-image)))

    (8-bit-rgb-image
     (destructuring-bind (height width channels)
         (array-dimensions image)
       (declare (ignore channels))
       (let ((tiff-image (make-instance 'tiff:tiff-image
                                        :width width
                                        :length height
                                        :bits-per-sample '(8 8 8)
                                        :samples-per-pixel 3
                                        :data (make-array (* width height 3)))))
         (with-accessors ((image-data tiff:tiff-image-data))
             tiff-image
           (loop for i below height
              do 
              (loop for j below width
                 do 
                 (let ((pixoff (* 3 (+ (* i width) j))))
                   (multiple-value-bind
                         (r g b)
                       (pixel image i j)
                     (setf (aref image-data pixoff) r
                           (aref image-data (incf pixoff)) g
                           (aref image-data (incf pixoff)) b))))))
         tiff-image)))
    
    (8-bit-rgba-image
     (destructuring-bind (height width channels)
         (array-dimensions image)
       (declare (ignore channels))
       (let ((tiff-image (make-instance 'tiff:tiff-image
                                        :width width
                                        :length height
                                        :bits-per-sample '(8 8 8 8)
                                        :samples-per-pixel 4
                                        :data (make-array (* width height 4)))))
         (with-accessors ((image-data tiff:tiff-image-data))
             tiff-image
           (loop for i below height
              do 
              (loop for j below width
                 do 
                 (let ((pixoff (* 4 (+ (* i width) j))))
                   (multiple-value-bind
                         (r g b a)
                       (pixel image i j)
                     (setf (aref image-data pixoff) r
                           (aref image-data (incf pixoff)) g
                           (aref image-data (incf pixoff)) b
                           (aref image-data (incf pixoff)) a))))))
         tiff-image)))

    (16-bit-rgb-image
     (destructuring-bind (height width channels)
         (array-dimensions image)
       (declare (ignore channels))
       (let ((tiff-image (make-instance 'tiff:tiff-image
                                        :width width
                                        :length height
                                        :bits-per-sample '(16 16 16)
                                        :samples-per-pixel 3
                                        :data (make-array (* width height 3 2)))))
         (with-accessors ((image-data tiff:tiff-image-data))
             tiff-image
           (loop for i below height
              do 
                (loop for j below width
                   do 
                     (let ((pixoff (* 3 2 (+ (* i width) j))))
                       (multiple-value-bind
                             (r g b)
                           (pixel image i j)
                         (setf (aref image-data pixoff) (ash r -8)
                               (aref image-data (incf pixoff)) (logand r #xff)

                               (aref image-data (incf pixoff)) (ash g -8)
                               (aref image-data (incf pixoff)) (logand g #xff)
                               
                               (aref image-data (incf pixoff)) (ash b -8)
                               (aref image-data (incf pixoff)) (logand b #xff)))))))
         tiff-image)))

    (16-bit-rgba-image
     (destructuring-bind (height width channels)
         (array-dimensions image)
       (declare (ignore channels))
       (let ((tiff-image (make-instance 'tiff:tiff-image
                                        :width width
                                        :length height
                                        :bits-per-sample '(16 16 16 16)
                                        :samples-per-pixel 4
                                        :data (make-array (* width height 4 2)))))
         (with-accessors ((image-data tiff:tiff-image-data))
             tiff-image
           (loop for i below height
              do 
                (loop for j below width
                   do 
                     (let ((pixoff (* 4 2 (+ (* i width) j))))
                       (multiple-value-bind
                             (r g b a)
                           (pixel image i j)
                         (setf (aref image-data pixoff) (ash r -8)
                               (aref image-data (incf pixoff)) (logand r #xff)

                               (aref image-data (incf pixoff)) (ash g -8)
                               (aref image-data (incf pixoff)) (logand g #xff)
                               
                               (aref image-data (incf pixoff)) (ash b -8)
                               (aref image-data (incf pixoff)) (logand b #xff)
                               
                               (aref image-data (incf pixoff)) (ash a -8)
                               (aref image-data (incf pixoff)) (logand a #xff)))))))
         tiff-image)))

    (t (error "Cannot write a TIFF image from ~A" (type-of image)))))

(defun write-tiff-stream (stream image &key byte-order)
  (let ((tiff-image (make-tiff-image image)))
    (apply #'tiff:write-tiff-stream stream tiff-image
           (when byte-order `(:byte-order ,byte-order)))))

(defun write-tiff-file (pathname image &key byte-order)
  (let ((tiff-image (make-tiff-image image)))
    (apply #'tiff:write-tiff-file pathname tiff-image
           :if-exists :supersede
           (when byte-order `(:byte-order ,byte-order)))))

