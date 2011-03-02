;;; Copyright (c) 2011 Cyrus Harmon, All rights reserved.
;;; See COPYRIGHT file for details.

(in-package :opticl)

(defun normalize-array (array)
  (let ((sum (coerce (sum array) 'double-float)))
    (when (not (zerop sum))
      (map-array (lambda (n) (/ n sum)) array))))

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
    (with-image-bounds (ur uc channels) u
      (with-image-bounds (vr vc) v
        ;; need a new matrix z to hold the values of the convolved matrix
        ;; dim z should be dim u + dim v - 1
        (let ((zr (+ ur vr -1))
              (zc (+ uc vc -1)))
          (let ((z (make-array (apply #'list zr zc
                                      (when channels (list channels)))
                               :element-type (array-element-type u))))
            (if channels
                (loop for channel below channels
                   do 
                   (dotimes (i zr)
                     (let ((ustartr (max 0 (- i vr -1)))
                           (uendr (min (- ur 1) i))
                           (vstartr (- vr (max (- vr i) 1))))
                       (dotimes (j zc)
                         (let ((ustartc (max 0 (- j vc -1)))
                               (uendc (min (- uc 1) j))
                               (vstartc (- vc (max (- vc j) 1)))
                               (acc 0))
                           (do ((urow ustartr (1+ urow))
                                (vrow vstartr (1- vrow)))
                               ((> urow uendr))
                             (do ((ucol ustartc (1+ ucol))
                                  (vcol vstartc (1- vcol)))
                                 ((> ucol uendc))
                               (let ((uv (aref u urow ucol channel))
                                     (vv (aref v vrow vcol)))
                                 (incf acc (* uv vv)))))
                           (setf (aref z i j channel) (funcall fit-function acc)))))))
                (dotimes (i zr)
                  (let ((ustartr (max 0 (- i vr -1)))
                        (uendr (min (- ur 1) i))
                        (vstartr (- vr (max (- vr i) 1))))
                    (dotimes (j zc)
                      (let ((ustartc (max 0 (- j vc -1)))
                            (uendc (min (- uc 1) j))
                            (vstartc (- vc (max (- vc j) 1)))
                            (acc 0))
                        (do ((urow ustartr (1+ urow))
                             (vrow vstartr (1- vrow)))
                            ((> urow uendr))
                          (do ((ucol ustartc (1+ ucol))
                               (vcol vstartc (1- vcol)))
                              ((> ucol uendc))
                            (let ((uv (aref u urow ucol))
                                  (vv (aref v vrow vcol)))
                              (incf acc (* uv vv)))))
                        (setf (aref z i j) (funcall fit-function acc)))))))
            z))))))


(defparameter *gaussian-kernel*
  (normalize-array #2A((1 2 1)
                       (2 4 2)
                       (1 2 1))))
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

