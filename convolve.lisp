;;; Copyright (c) 2011 Cyrus Harmon, All rights reserved.
;;; See COPYRIGHT file for details.

(in-package :opticl)

(defun normalize-array (array &key element-type)
  (let ((sum (coerce (sum array) 'double-float)))
    (if (zerop sum)
        array
        (apply #'map-array (lambda (n) (/ n sum)) array
               :force-simple t
               (when element-type `(:element-type ,element-type))))))


(defun discrete-convolve (u v)
  "Perform a discrete convolution of matrix u with matrix v"
  (let ((fit-function
         (let ((type (array-element-type u)))
           (cond
             ((equal type '(unsigned-byte 1))
              (make-constrain-fn 0 1))
             ((equal type '(unsigned-byte 2))
              (make-constrain-fn 0 3))
             ((equal type '(unsigned-byte 4))
              (make-constrain-fn 0 15))
             ((equal type '(unsigned-byte 8))
              (make-constrain-fn 0 255))
             ((equal type '(unsigned-byte 16))
              (make-constrain-fn 0 #xffff))
             ((equal type '(unsigned-byte 32))
              (make-constrain-fn 0 #xffffffff))
             (t #'identity)))))
    (with-image-bounds (vr vc) v
      (unless (and (= vr vc)
                   (oddp vr))
        (error "convolution matrix bust be square and of odd dimensions"))
      (let ((span (ash vr -1)))
        (declare (type fixnum span))
        (with-image-bounds (ur uc channels) u
          (let ((zr (+ ur vr -1))
                (zc (+ uc vc -1)))
            (let ((z (make-array (apply #'list zr zc
                                        (when channels (list channels)))
                                 :element-type (array-element-type u))))
              ;; let's speed this up for some common cases:
              (cond ((and (typep u '8-bit-rgb-image)
                          (typep v 'double-float-gray-image))
                     (locally
                         (declare (type 8-bit-rgb-image u z)
                                  (type double-float-gray-image v)
                                  (type fixnum span)
                                  (optimize (speed 3)))
                       (do-pixels (i j) z
                         (let ((acc-r 0d0) (acc-g 0d0) (acc-b 0d0))
                           (declare (type double-float acc-r acc-g acc-b ))
                           (loop for ui fixnum from (- i span) to (+ i span)
                              for vi fixnum downfrom (1- vr) downto 0
                              do (loop for uj fixnum from (- j span) to (+ j span)
                                    for vj fixnum downfrom (1- vc) downto 0
                                    do 
                                      (let ((ui* (constrain ui 0 (1- ur)))
                                            (uj* (constrain uj 0 (1- uc)))
                                            (v-val (aref v vi vj)))
                                        (declare (type double-float v-val))
                                        (multiple-value-bind (r g b) (pixel u ui* uj*)
                                          (declare (type (unsigned-byte 8) r g b))
                                          (setf acc-r (+ acc-r (* r v-val)))
                                          (setf acc-g (+ acc-g (* g v-val)))
                                          (setf acc-b (+ acc-b (* b v-val)))))))
                           (setf (pixel z i j) (values 
                                                (cond ((<= acc-r 0d0) 0)
                                                      ((>= acc-r 255d0) 255)
                                                      (t (round acc-r)))
                                                (cond ((<= acc-g 0d0) 0)
                                                      ((>= acc-g 255d0) 255)
                                                      (t (round acc-g)))
                                                (cond ((<= acc-b 0d0) 0)
                                                      ((>= acc-b 255d0) 255)
                                                      (t (round acc-b)))))))))
                    (t
                     (do-pixels (i j) z
                       (let ((acc (if channels
                                      (make-list channels :initial-element 0)
                                      0)))
                         (loop for ui fixnum from (- i span) to (+ i span)
                            for vi fixnum downfrom (1- vr) downto 0
                            do (loop for uj fixnum from (- j span) to (+ j span)
                                  for vj fixnum downfrom (1- vc) downto 0
                                  do 
                                  (let ((ui* (constrain ui 0 (1- ur)))
                                        (uj* (constrain uj 0 (1- uc))))
                                    (if channels
                                        (setf acc (mapcar (lambda (a q) (+ a (* q (aref v vi vj))))
                                                          acc
                                                          (multiple-value-list (pixel u ui* uj*))))
                                        (incf acc (* (pixel u ui* uj*) (aref v vi vj)))))))
                         (if channels
                             (setf (pixel z i j) (values-list (mapcar fit-function acc)))
                             (setf (pixel z i j) (funcall fit-function acc)))))))
              z)))))))

(defparameter *gaussian-kernel*
  (normalize-array #2A((1 2 1)
                       (2 4 2)
                       (1 2 1))
                   :element-type 'double-float))
(defun blur-image (img)
  (trim-image
   (discrete-convolve img *gaussian-kernel*) 1 1))

(defparameter *sharpen-kernel*
  (normalize-array #2A((-1 -4 -1)
                       (-4 26 -4)
                       (-1 -4 -1))))

(defun sharpen-image (img)
  (trim-image
   (discrete-convolve img *sharpen-kernel*) 1 1))

