
(in-package #:opticl-test)

(defparameter *circles* (test-circles))
(defparameter *transformed*
  (let ((xfrm (make-affine-transformation :theta 0.2d0))
        (new-matrix (make-8-bit-rgb-image 480 640)))
    (transform-image *circles* new-matrix xfrm)))

(write-png-file "test/output/circles2.png" *circles*)
(write-png-file "test/output/transformed-circles2.png" *transformed*)

(defparameter *q*
  (make-8-bit-gray-image 4 4 :initial-contents '((1 2 3 4) (5 6 7 8) (9 10 11 12) (13 14 15 16))))

(defparameter *q-prime*
  (let ((xfrm (make-affine-transformation :x-scale 2.0d0))
        (new-matrix (make-8-bit-gray-image 4 4)))
    (transform-image *q* new-matrix xfrm :u '(-2d0 . 2d0) :v '(-2d0 . 2d0) :x '(-2d0 . 2d0) :y '(-2d0 . 2d0))))

(write-png-file "test/output/resized-circles2.png"
                (resize-image *circles* 960 1280))

