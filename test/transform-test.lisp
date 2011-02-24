
(in-package #:opticl-test)

(assert
 (equalp '(10d0 10d0)
         (multiple-value-list
          (let ((y 0) (x 0)
                (xfrm (make-affine-transformation :y-shift 10 :x-shift 10)))
            (opticl::transform-coord y x xfrm)))))

(assert
 (equalp '(20d0 10d0)
         (multiple-value-list
          (let ((y 10) (x 0)
                (xfrm (make-affine-transformation :y-shift 10 :x-shift 10)))
            (opticl::transform-coord y x xfrm)))))

(assert
 (equalp '(30d0 90d0)
         (multiple-value-list
          (let ((y 10) (x 20)
                (xfrm (make-affine-transformation :y-shift 10 :x-shift 10
                                                  :y-scale 2 :x-scale 4)))
            (opticl::transform-coord y x xfrm)))))

(multiple-value-list
  (let ((y 0) (x 10)
        (xfrm (make-affine-transformation :theta (/ pi 2))))
    (opticl::transform-coord y x xfrm)))

(multiple-value-list
  (let ((y 0) (x 10)
        (xfrm (make-affine-transformation :x-shear 2)))
    (opticl::transform-coord y x xfrm)))

(multiple-value-list
  (let ((y 10) (x 10)
        (xfrm (make-affine-transformation :x-shear 1)))
    (opticl::transform-coord y x xfrm)))

(multiple-value-list
  (let ((y 10) (x 10)
        (xfrm (make-affine-transformation :y-shear 1)))
    (opticl::transform-coord y x xfrm)))


(defparameter *circles* (test-circles))
(defparameter *transformed*
  (let ((xfrm (make-affine-transformation :theta 0.2d0)))
    (time (transform-image *circles* xfrm :interpolate :bilinear))))

(write-png-file "test/output/circles2.png" *circles*)
(write-png-file "test/output/transformed-circles2.png" *transformed*)

(defparameter *q*
  #+nil (make-8-bit-gray-image 4 4 :initial-contents '((1 2 3 4) (5 6 7 8) (9 10 11 12) (13 14 15 16)))
  
  (make-8-bit-gray-image 64 64
                         :initial-element 128
                         #+nil :initial-contents
                         #+nil (loop for i below 16
                                  collect (loop for j below 16
                                             collect (* i j)))))

(defparameter *q-prime*
  (let ((xfrm (make-affine-transformation :theta 0.2d0)))
    (transform-image *q* xfrm :u '(-4d0 . 4d0) :v '(-4d0 . 4d0) :x '(-4d0 . 4d0) :y '(-4d0 . 4d0) :interpolate :bilinear)))

(write-png-file "test/output/resized-circles2.png" (resize-image *circles* 960 1280))

(write-png-file "test/output/q-prime.png" *q-prime*)



(defparameter *cropped-salad*
  (let ((img (read-jpeg-file "test/images/salad.jpg")))
    (let ((cropped (opticl::crop-image img 400 200 600 400)))
      (write-png-file "test/output/cropped-salad.png" cropped)
      cropped)))

(let ((transform (make-affine-transformation :x-scale 1.5d0 :y-scale 1.5d0)))
  (let ((bigimg
         (transform-image *cropped-salad* transform :interpolate :bilinear)))
    (write-png-file "test/output/salad-big.jpg" bigimg)))

(let ((transform (make-affine-transformation
                  :x-scale 1.2d0 :y-scale 1.1d0
                  :x-shear 1.3d0 :y-shear 1.8d0
                  :theta (* -45.0d0 (/ 180.0d0) pi)
                  :x-shift 40 :y-shift 40)))
  (let ((transimg
         (transform-image *cropped-salad* transform :interpolate :bilinear)))
    (write-png-file "test/output/salad-trans.jpg" transimg)))

(let ((transform (make-affine-transformation
                  :theta (/ pi 8)
                  ;; :x-shift 200
                  :theta (* 45.0d0 (/ 180.0d0) pi)
                  )))
  (let ((transimg
         (transform-image *cropped-salad* transform :interpolate :bilinear)))
    (write-png-file "test/output/salad-trans.jpg" transimg)))

(let ((transform (make-affine-transformation :x-scale 2d0 :y-scale 2d0)))
  (let ((transimg
         (transform-image *cropped-salad* transform :interpolate :bilinear)))
    (write-png-file "test/output/cropped-salad-double.jpg" transimg)))


