
(in-package #:opticl)

#+nil
(defun fill-image-8-bit-rgb (img r g b)
  (declare (type (unsigned-byte 8) r g b))
  (destructuring-bind (height width channels)
      (array-dimensions img)
    (declare (ignore channels))
    (loop for i below height
       do (loop for j below width 
             do 
               (setf (pixel/8-bit-rgb-image img i j)
                     (values r g b))))))

(macrolet
    ((frob-fill-image-rgb (bits channel-names)
       (let ((function-name
              (intern (string-upcase (format nil "fill-image-~A-bit-rgb" bits))))
             (pixel-function
              (intern (string-upcase (format nil "pixel/~A-bit-rgb-image" bits)))))
         `(defun ,function-name (img ,@channel-names)
            (declare (type (unsigned-byte ,bits) ,@channel-names))
            (destructuring-bind (height width channels)
                (array-dimensions img)
              (declare (ignore channels))
              (loop for i below height
                 do (loop for j below width 
                       do 
                       (setf (,pixel-function img i j)
                             (values ,@channel-names)))))))))
  (frob-fill-image-rgb 4 (r g b))
  (frob-fill-image-rgb 8 (r g b))
  (frob-fill-image-rgb 16 (r g b)))

(defun horizontal-line-8-bit-rgb (img y x0 x1 r g b)
  (declare (type fixnum y x0 x1))
  (loop for x fixnum from x0 to x1
     do (setf (pixel/8-bit-rgb-image img y x) (values r g b))))

(defun vertical-line-8-bit-rgb (img y0 y1 x r g b)
  (declare (type fixnum y0 y1 x))
  (loop for y fixnum from y0 to y1
     do (setf (pixel/8-bit-rgb-image img y x) (values r g b))))

(defun draw-line-8-bit-rgb (img y0 x0 y1 x1 r g b)
  (declare (type 8-bit-rgb-image img)
           (type fixnum y0 x0 y1 x1))
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
              (setf (pixel/8-bit-rgb-image img y x) (values r g b))
              (dotimes (i absdx)
                (cond
                  ((<= d 0)
                   (incf d incr-e)
                   (incf x xstep))
                  (t
                   (incf d incr-ne)
                   (incf x xstep)
                   (incf y ystep)))
                (setf (pixel/8-bit-rgb-image img y x) (values r g b))))
            (let ((d (- (* 2 absdy) absdx))
                  (incr-n (* 2 absdx))
                  (incr-ne (* 2 (- absdx absdy)))
                  (x x0)
                  (y y0))
              (declare (type fixnum d incr-n incr-ne x y))
              (setf (pixel/8-bit-rgb-image img y x) (values r g b))
              (dotimes (i absdy)
                (cond
                  ((<= d 0)
                   (incf d incr-n)
                   (incf y ystep))
                  (t
                   (incf d incr-ne)
                   (incf y ystep)
                   (incf x xstep)))
                (setf (pixel/8-bit-rgb-image img y x) (values r g b)))))))))

(defun draw-circle-8-bit-rgb (img center-y center-x radius r g b)
  "draws a circle centered at (x, y) with radius r on a image."
  (declare (type 8-bit-rgb-image img)
           (type fixnum center-y center-x radius))
  (flet ((circle-points (y x r g b)
           (setf (pixel/8-bit-rgb-image img (+ center-y y) (+ center-x x)) (values r g b))
           (setf (pixel/8-bit-rgb-image img (+ center-y x) (+ center-x y)) (values r g b))
           (setf (pixel/8-bit-rgb-image img (- center-y x) (+ center-x y)) (values r g b))
           (setf (pixel/8-bit-rgb-image img (- center-y y) (+ center-x x)) (values r g b))
           (setf (pixel/8-bit-rgb-image img (- center-y y) (- center-x x)) (values r g b))
           (setf (pixel/8-bit-rgb-image img (- center-y x) (- center-x y)) (values r g b))
           (setf (pixel/8-bit-rgb-image img (+ center-y x) (- center-x y)) (values r g b))
           (setf (pixel/8-bit-rgb-image img (+ center-y y) (- center-x x)) (values r g b))))
    (let ((x 0)
          (y radius)
          (d (- 1 radius))
          (delta-e 3)
          (delta-se (+ (* -2 radius) 5)))
      (declare (type fixnum x y d delta-e delta-se))
      (circle-points y x r g b)
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
        (circle-points y x r g b)))))

(defun fill-circle-8-bit-rgb (img center-y center-x radius r g b)
  "draws a filled circle centered at (x, y) with radius r on a image."
  (declare (type fixnum center-y center-x radius))
  (flet ((circle-lines (y x)
           (horizontal-line-8-bit-rgb img (- center-y y) (- center-x x) (+ center-x x) r g b)
           (horizontal-line-8-bit-rgb img (- center-y x) (- center-x y) (+ center-x y) r g b)
           (horizontal-line-8-bit-rgb img (+ center-y y) (- center-x x) (+ center-x x) r g b)
           (horizontal-line-8-bit-rgb img (+ center-y x) (- center-x y) (+ center-x y) r g b)))
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

(defun draw-rectangle-8-bit-rgb (img y0 x0 y1 x1 r g b)
  (horizontal-line-8-bit-rgb img y0 x0 x1 r g b)
  (vertical-line-8-bit-rgb img y0 y1 x0 r g b)
  (vertical-line-8-bit-rgb img y0 y1 x1 r g b)
  (horizontal-line-8-bit-rgb img y1 x0 x1 r g b))

(defun fill-rectangle-8-bit-rgb (img y0 x0 y1 x1 r g b)
  (loop for x from x0 to x1
     do 
       (vertical-line-8-bit-rgb img y0 y1 x r g b)))

(defun draw-triangle-8-bit-rgb (img y0 x0 y1 x1 y2 x2 r g b)
  (draw-line-8-bit-rgb img y0 x0 y1 x1 r g b)
  (draw-line-8-bit-rgb img y1 x1 y2 x2 r g b)
  (draw-line-8-bit-rgb img y2 x2 y0 x0 r g b))

(defun draw-polygon-8-bit-rgb (img points r g b)
  (loop for p across points
     do (let ((p1 (elt p 0))
              (p2 (elt p 1)))
          (when (and (consp p1) (consp p2))
            (draw-line-8-bit-rgb img (car p1) (cdr p1) (car p2) (cdr p2) r g b)))))
