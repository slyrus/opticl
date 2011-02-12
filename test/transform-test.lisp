
(in-package #:opticl-test)

(defparameter *circles* (test-circles))
(defparameter *transformed*
  (let ((xfrm (opticl::make-affine-transformation :theta 0.2d0))
        (new-matrix (make-8-bit-rgb-image 480 640)))
    (opticl::transform-image *circles* new-matrix xfrm)))

(write-png-file "test/output/circles2.png" *circles*)
(write-png-file "test/output/transformed-circles2.png" *transformed*)

(defparameter *q*
  (make-8-bit-gray-image 4 4 :initial-contents '((1 2 3 4) (5 6 7 8) (9 10 11 12) (13 14 15 16))))

(defparameter *q-prime*
  (let ((xfrm (opticl::make-affine-transformation :x-scale 2.0d0))
        (new-matrix (make-8-bit-gray-image 4 4)))
    (opticl::transform-image *q* new-matrix xfrm :u '(-2d0 . 2d0) :v '(-2d0 . 2d0) :x '(-2d0 . 2d0) :y '(-2d0 . 2d0))))

(defun split-around-zero (k &key integer)
  (let ((khalf (/ k 2.0d0)))
    (if integer
        (cons (floor (- khalf)) (ceiling khalf))
        (cons (+ (- khalf) 0.5d0) (+ khalf 0.5d0)))))

(defun resize-image (img y x)
  (with-image-bounds (oldy oldx)
      img
    (let ((yscale (/ y oldy))
          (xscale (/ x oldx)))
      (let ((xfrm (opticl::make-affine-transformation :x-scale xscale :y-scale yscale))
            (new-image (make-8-bit-gray-image y x)))
        (let ((n (opticl::transform-image
                  img new-image xfrm
                  :u (split-around-zero oldy)
                  :v (split-around-zero oldx)
                  :y (split-around-zero y)
                  :x (split-around-zero x))))
          n)))))
