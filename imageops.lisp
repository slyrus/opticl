;;; Copyright (c) 2011 Cyrus Harmon, All rights reserved.
;;; See COPYRIGHT file for details.

(in-package :opticl)

(defun constrain (val min max)
  (let ((val (if (< val min) min val)))
    (if (> val max)
        max
        val)))

(defun pixel-in-bounds (img y x)
  (with-image-bounds (ymax xmax)
      img
    (and (>= y 0) (< y ymax)
         (>= x 0) (< x xmax))))

(defmacro when-pixel-in-bounds ((img y x) &body body)
  (let ((ymax (gensym)) (xmax (gensym)))
    `(let ((,ymax (1- (array-dimension ,img 0)))
           (,xmax (1- (array-dimension ,img 1))))
       (if (and (<= 0 ,y ,ymax)
                (<= 0 ,x ,xmax))
           ,@body))))

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
    (let ((new-image (make-array
                 (cons ymax (cons xmax (when channels (list channels))))
                 :element-type (array-element-type img))))
      (loop for i below ymax
         do (loop for j below xmax
               do (setf (pixel new-image i j) (pixel img i j))))
      new-image)))

(defun crop-image (img y1 x1 y2 x2)
  (with-image-bounds (ymax xmax channels)
      img
    (let ((new-image (make-array
                      (cons ymax (cons xmax (when channels (list channels))))
                      :element-type (array-element-type img)))
          (new-rows (1+ (- y2 y1)))
          (new-cols (1+ (- x2 x1))))
      (loop for i-src from y1 below y2
         for i-dest below new-rows
         do (loop for j-src from x1 below x2
               for j-dest below new-cols
               do 
                 (setf (pixel new-image i-dest j-dest) (pixel img i-src j-src))))
      new-image)))

(defun map-array (fn array)
  (let* ((len (reduce #'* (array-dimensions array)))
         (elt-type (array-element-type array))
         (disp (make-array len
                           :element-type elt-type
                           :displaced-to array)))
    (make-array (array-dimensions array)
                :element-type elt-type
                :displaced-to (map `(vector ,elt-type) fn disp))))
