
(in-package #:opticl-test)

(defparameter *circles* (test-circles))
(defparameter *transformed*
  (let ((xfrm (make-affine-transformation :theta 0.2d0))
        (new-matrix (make-8-bit-rgb-image 480 640)))
    (time (transform-image *circles* new-matrix xfrm :interpolate :bilinear))))

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
  (let ((xfrm (make-affine-transformation :theta 0.2d0))
        (new-matrix (make-8-bit-gray-image 64 64)))
    (transform-image *q* new-matrix xfrm :u '(-4d0 . 4d0) :v '(-4d0 . 4d0) :x '(-4d0 . 4d0) :y '(-4d0 . 4d0) :interpolate :bilinear)))

(write-png-file "test/output/resized-circles2.png"
                (resize-image *circles* 960 1280))

(write-png-file "test/output/q-prime.png" *q-prime*)
