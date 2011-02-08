;;; Copyright (c) 2011 Cyrus Harmon, All rights reserved.
;;; See COPYRIGHT file for details.

(in-package :opticl)

(defun transpose-image (img)
  (with-image-bounds (ymax xmax channels)
      img
    (let ((zimg (make-array
                 (cons xmax (cons ymax (when channels (list channels))))
                 :element-type (array-element-type img))))
      (loop for i below ymax
           do (loop for j below xmax
                 do (setf (pixel zimg j i) (pixel img i j))))
      zimg)))

(defun copy-image (img)
  (with-image-bounds (ymax xmax channels)
      img
    (let ((zimg (make-array
                 (cons ymax (cons xmax (when channels (list channels))))
                 :element-type (array-element-type img))))
      (loop for i below ymax
         do (loop for j below xmax
               do (setf (pixel zimg i j) (pixel img i j))))
      zimg)))

