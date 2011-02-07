
(in-package #:opticl)

;;; We have two approaches for twiddling the bits in an opticl
;;; image. We can either use the slow generic functions, or we can use
;;; the fast pixel/<n>-bit-<type>-image functions. We explore a bit of
;;; both of those approaches here.
;;;
;;; for fill image, we use some macro hackery to generate fast
;;; functions for each of the image types, and then define a helper
;;; function to dispatch to the right fast function, based on the type
;;; of the image (vector) passed in. Note that we can't do CLOS
;;; dispatch on the particular specialized array type, or this would
;;; be a lot cleaner...
;;;


(macrolet
    ((frob-fill-image (type bits channel-names)
       (let ((function-name
              (intern (string-upcase (format nil "fill-image/~A" (symbol-name type)))))
             (pixel-function
              (intern (string-upcase (format nil "pixel/~A" (symbol-name type))))))
         `(defun ,function-name (img ,@channel-names)
            (declare (type (unsigned-byte ,bits) ,@channel-names))
            (destructuring-bind (height width &optional channels)
                (array-dimensions img)
              (declare (ignore channels))
              (loop for i below height
                 do (loop for j below width 
                       do 
                         (setf (,pixel-function img i j)
                               (values ,@channel-names)))))))))
  (frob-fill-image 1-bit-gray-image 1 (k))
  (frob-fill-image 2-bit-gray-image 2 (k))
  (frob-fill-image 4-bit-gray-image 4 (k))
  (frob-fill-image 8-bit-gray-image 8 (k))
  (frob-fill-image 16-bit-gray-image 16 (k))
  
  (frob-fill-image 4-bit-rgb-image 4 (r g b))
  (frob-fill-image 8-bit-rgb-image 8 (r g b))
  (frob-fill-image 16-bit-rgb-image 16 (r g b))
  
  (frob-fill-image 4-bit-rgba-image 4 (r g b a))
  (frob-fill-image 8-bit-rgba-image 8 (r g b a))
  (frob-fill-image 16-bit-rgba-image 16 (r g b a)))

(defun fill-image (img &rest vals)
  (etypecase img
    (1-bit-gray-image (apply #'fill-image/1-bit-gray-image img vals))
    (2-bit-gray-image (apply #'fill-image/2-bit-gray-image img vals))
    (4-bit-gray-image (apply #'fill-image/4-bit-gray-image img vals))
    (8-bit-gray-image (apply #'fill-image/8-bit-gray-image img vals))
    (16-bit-gray-image (apply #'fill-image/16-bit-gray-image img vals))
    
    (4-bit-rgb-image (apply #'fill-image/4-bit-rgb-image img vals))
    (8-bit-rgb-image (apply #'fill-image/8-bit-rgb-image img vals))
    (16-bit-rgb-image (apply #'fill-image/16-bit-rgb-image img vals))

    (4-bit-rgba-image (apply #'fill-image/4-bit-rgba-image img vals))
    (8-bit-rgba-image (apply #'fill-image/8-bit-rgba-image img vals))
    (16-bit-rgba-image (apply #'fill-image/16-bit-rgba-image img vals))))

;;;
;;; For the remaining functions, we take the slow approach. These
;;; should get replaced with fast paths, as exmplified by fill-image
;;; above.

(defun constrain (val min max)
  (let ((val (if (< val min) min val)))
    (if (> val max)
        max
        val)))

(defmacro with-image-bounds ((ymax-var xmax-var) img &body body)
  `(let ((,ymax-var (1- (array-dimension ,img 0)))
         (,xmax-var (1- (array-dimension ,img 1))))
     ,@body))

(defun horizontal-line (img y x0 x1 &rest vals)
  (declare (type fixnum y x0 x1))
  (with-image-bounds (ymax xmax)
      img
    (let ((y (constrain y 0 ymax))
          (x0 (constrain x0 0 xmax))
          (x1 (constrain x1 0 xmax)))
      (loop for x fixnum from x0 to x1
         do (setf (pixel img y x) (values-list vals))))))

(defun pixel-in-bounds (img y x)
  (with-image-bounds (ymax xmax)
      img
    (and (>= y 0)
         (< y ymax)
         (>= x 0)
         (< x xmax))))

(defun vertical-line (img y0 y1 x &rest vals)
  (declare (type fixnum y0 y1 x))
  (with-image-bounds (ymax xmax)
      img
    (let ((y0 (constrain y0 0 ymax))
          (y1 (constrain y1 0 xmax))
          (x (constrain x 0 xmax)))
      (loop for y fixnum from y0 to y1
         do (when (pixel-in-bounds img y x)
              (setf (pixel img y x) (values-list vals)))))))

(defun draw-line (img y0 x0 y1 x1 &rest vals)
  (declare (type fixnum y0 x0 y1 x1))
  (let ((dx (- x1 x0))
        (dy (- y1 y0)))
    (declare (type fixnum dx dy))
    (let ((absdx (abs dx))
          (absdy (abs dy)))
      (declare (type fixnum absdx absdy))
      (let ((xstep (if (minusp dx) -1 1))
            (ystep (if (minusp dy) -1 1)))
        (if (>= absdx absdy)
            (let ((d (- (* 2 absdy) absdx))
                  (incr-e (* 2 absdy))
                  (incr-ne (* 2 (- absdy absdx)))
                  (x x0)
                  (y y0))
              (declare (type fixnum d incr-e incr-ne x y))
              (when (pixel-in-bounds img y x)
                (setf (pixel img y x) (values-list vals)))
              (dotimes (i absdx)
                (cond
                  ((<= d 0)
                   (incf d incr-e)
                   (incf x xstep))
                  (t
                   (incf d incr-ne)
                   (incf x xstep)
                   (incf y ystep)))
                (when (pixel-in-bounds img y x)
                  (setf (pixel img y x) (values-list vals)))))
            (let ((d (- (* 2 absdy) absdx))
                  (incr-n (* 2 absdx))
                  (incr-ne (* 2 (- absdx absdy)))
                  (x x0)
                  (y y0))
              (declare (type fixnum d incr-n incr-ne x y))
                (when (pixel-in-bounds img y x)
                  (setf (pixel img y x) (values-list vals)))
              (dotimes (i absdy)
                (cond
                  ((<= d 0)
                   (incf d incr-n)
                   (incf y ystep))
                  (t
                   (incf d incr-ne)
                   (incf y ystep)
                   (incf x xstep)))
                (when (pixel-in-bounds img y x)
                  (setf (pixel img y x) (values-list vals))))))))))

(defun draw-circle (img center-y center-x radius &rest vals)
  "draws a circle centered at (x, y) with radius r on a image."
  (declare (type fixnum center-y center-x radius))
  (flet ((pixel-if (y x vals)
           (when (pixel-in-bounds img y x)
             (setf (pixel img y x) (values-list vals)))))
    (flet ((circle-points (y x)
             (pixel-if (+ center-y y) (+ center-x x) vals)
             (pixel-if (+ center-y x) (+ center-x y) vals)
             (pixel-if (- center-y x) (+ center-x y) vals)
             (pixel-if (- center-y y) (+ center-x x) vals)
             (pixel-if (- center-y y) (- center-x x) vals)
             (pixel-if (- center-y x) (- center-x y) vals)
             (pixel-if (+ center-y x) (- center-x y) vals)
             (pixel-if (+ center-y y) (- center-x x) vals)))
      (let ((x 0)
            (y radius)
            (d (- 1 radius))
            (delta-e 3)
            (delta-se (+ (* -2 radius) 5)))
        (declare (type fixnum x y d delta-e delta-se))
        (circle-points y x)
        (do () ((>= x y))
          (if (< d 0)
              (progn
                (incf d delta-e)
                (incf delta-e 2)
                (incf delta-se 2))
              (progn
                (incf d delta-se)
                (incf delta-e 2)
                (incf delta-se 4)
                (decf y)))
          (incf x)
          (circle-points y x))))))

(defun fill-circle (img center-y center-x radius &rest vals)
  "draws a filled circle centered at (x, y) with radius r on a image."
  (declare (type fixnum center-y center-x radius))
  (flet ((circle-lines (y x)
           (apply #'horizontal-line img (- center-y y) (- center-x x) (+ center-x x) vals)
           (apply #'horizontal-line img (- center-y x) (- center-x y) (+ center-x y) vals)
           (apply #'horizontal-line img (+ center-y y) (- center-x x) (+ center-x x) vals)
           (apply #'horizontal-line img (+ center-y x) (- center-x y) (+ center-x y) vals)))
    (let ((x 0)
          (y radius)
          (d (- 1 radius))
          (delta-e 3)
          (delta-se (+ (* -2 radius) 5)))
      (declare (type fixnum x y d delta-e delta-se))
      (circle-lines y x)
      (do () ((>= x y))
        (if (< d 0)
            (progn
              (incf d delta-e)
              (incf delta-e 2)
              (incf delta-se 2))
            (progn
              (incf d delta-se)
              (incf delta-e 2)
              (incf delta-se 4)
              (decf y)))
        (incf x)
        (circle-lines y x)))))

(defun draw-rectangle (img y0 x0 y1 x1 &rest vals)
  (apply #'horizontal-line img y0 x0 x1 vals)
  (apply #'vertical-line img y0 y1 x0 vals)
  (apply #'vertical-line img y0 y1 x1 vals)
  (apply #'horizontal-line img y1 x0 x1 vals))

(defun fill-rectangle (img y0 x0 y1 x1 &rest vals)
  (loop for x from x0 to x1
     do 
       (apply #'vertical-line img y0 y1 x vals)))

(defun draw-triangle (img y0 x0 y1 x1 y2 x2 &rest vals)
  (apply #'draw-line img y0 x0 y1 x1 vals)
  (apply #'draw-line img y1 x1 y2 x2 vals)
  (apply #'draw-line img y2 x2 y0 x0 vals))

(defun draw-polygon (img points &rest vals)
  (loop for p across points
     do (let ((p1 (elt p 0))
              (p2 (elt p 1)))
          (when (and (consp p1) (consp p2))
            (apply #'draw-line img (car p1) (cdr p1) (car p2) (cdr p2) vals)))))
