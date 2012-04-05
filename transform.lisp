
(in-package :opticl)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +epsilon+ 0.00001d0))

(deftype affine-coord ()
  `(simple-array float (3)))

(deftype affine-transformation ()
  `(simple-array float (3 3)))

(defun make-coord (y x)
  (make-array 3
              :element-type 'double-float
              :initial-contents (list (coerce y 'double-float)
                                      (coerce x 'double-float)
                                      0d0)))

(defun make-affine-transformation (&key
                                   (y-shift 0d0) (x-shift 0d0)
                                   (theta 0d0)
                                   (y-scale 1d0) (x-scale 1d0)
                                   (y-shear 0d0) (x-shear 0d0))
  (let ((xfrm (make-array '(3 3)
                          :element-type 'double-float
                          :initial-element 0d0))
        (cos-theta (cos theta))
        (sin-theta (sin theta)))
    (setf (aref xfrm 0 0) (+ (* cos-theta y-scale)
                             (* sin-theta x-scale x-shear)))
    (setf (aref xfrm 0 1) (+ (* sin-theta x-scale)
                             (* cos-theta y-scale y-shear)))

    (setf (aref xfrm 1 0) (- (* cos-theta x-scale x-shear)
                             (* sin-theta y-scale)))
    (setf (aref xfrm 1 1) (- (* cos-theta x-scale)
                             (* sin-theta y-scale y-shear)))
    
    (setf (aref xfrm 0 2) (coerce y-shift 'double-float))
    (setf (aref xfrm 1 2) (coerce x-shift 'double-float))
    (setf (aref xfrm 2 2) 1d0)
    
    xfrm))

(defun matrix-multiply (matrix-a matrix-b)
  (destructuring-bind (matrix-a-rows matrix-a-columns)
      (array-dimensions matrix-a)
    (destructuring-bind (matrix-b-rows matrix-b-columns)
        (array-dimensions matrix-b)
      (if (= matrix-a-columns matrix-b-rows)
          (let* ((c (make-array (list matrix-a-rows matrix-b-columns)
                                :element-type (array-element-type matrix-a))))
            (dotimes (i matrix-a-rows)
              (dotimes (j matrix-b-columns)
                (let ((v 0))
                  (dotimes (r matrix-a-columns)
                    (incf v (* (aref matrix-a i r) (aref matrix-b r j))))
                  (setf (aref c i j) v))))
            c)))))

(defun post-multiply-by-column-vector (matrix-a column-vector)
  (destructuring-bind (matrix-a-rows matrix-a-columns)
      (array-dimensions matrix-a)
    (when (= (length column-vector) matrix-a-columns)
      (let* ((c (make-array matrix-a-rows
                            :element-type (array-element-type matrix-a))))
        (dotimes (i matrix-a-rows)
          (let ((v 0))
            (dotimes (r matrix-a-columns)
              (incf v (* (aref matrix-a i r) (aref column-vector r))))
            (setf (aref c i) v)))
        c))))

(defun transform-coord (y x xfrm)
  "applies the affine transformation xfrm to the point {x,y} and
  returns the position of the point after applying the transformation"
  (let ((coord1 (make-array 3
                            :element-type 'double-float
                            :initial-element 1d0)))
    (setf (aref coord1 0) (coerce y 'double-float)
          (aref coord1 1) (coerce x 'double-float))
    (let ((coord2 (post-multiply-by-column-vector xfrm coord1)))
      (values (aref coord2 0) (aref coord2 1)))))

(defun bilinear-interpolate (g00 g01 g10 g11 a b)
  (+ g00
     (* a (- g10 g00))
     (* b (- g01 g00))
     (* a b (- (+ g00 g11)
                 (+ g10 g01)))))

(defmacro quadratic-kernel (s type)
  (let ((minus-half (coerce -0.5 `,type))
        (half (coerce 0.5 `,type))
        (minus-one-point-five  (coerce -1.5 `,type))
        (one-point-five (coerce 1.5 `,type))
        (five (coerce 5 `,type))
        (minus-two (coerce -2 `,type))
        (two (coerce 2 `,type))
        (one (coerce 1 `,type))
        (zero (coerce 0 `,type)))
    `(cond ((<= ,minus-half ,s ,half)
            (+ (* ,minus-two (* ,s ,s)) ,one))
           ((<= ,minus-one-point-five ,s ,one-point-five)
            (+ (* ,s ,s) (- (/ (* ,five (abs ,s)) ,two)) ,one-point-five))
           (t ,zero))))

(defun quadratic-kernel-2 (s)
  (cond ((<= -.5d0 s .5d0)
         (+ (* -2d0 (* s s) 1d0)))
        ((<= -1.5d0 s 1.5d0)
         (+ (* s s) (- (/ (* 5 (abs s)) 2)) 1.5))
        (t 0d0)))

(defmacro quadratic-interpolate
    (g00 g01 g02
     g10 g11 g12 
     g20 g21 g22 a b
     &optional (type 'double-float))
  `(let ((a0 (quadratic-kernel (- -1 ,a) ,type))
         (a1 (quadratic-kernel (- ,a) ,type))
         (a2 (quadratic-kernel (- 1 ,a) ,type))
         (b0 (quadratic-kernel (- -1 ,b) ,type))
         (b1 (quadratic-kernel (- ,b) ,type))
         (b2 (quadratic-kernel (- 1 ,b) ,type)))
     (+ (* a0 (+ (* b0 ,g00)
                 (* b1 ,g01)
                 (* b2 ,g02)))
        (* a1 (+ (* b0 ,g10)
                 (* b1 ,g11)
                 (* b2 ,g12)))
        (* a2 (+ (* b0 ,g20)
                 (* b1 ,g21)
                 (* b2 ,g22))))))


(defun %fast-affine-transform-image (matrix-m matrix-n xfrm)
  (declare (optimize (speed 3)))
  (typecase matrix-m

    (8-bit-gray-image
     (locally
         (declare (type 8-bit-gray-image matrix-m matrix-n))
       (with-image-bounds (matrix-m-rows matrix-m-columns)
           matrix-m
         (with-image-bounds (matrix-n-rows matrix-n-columns)
             matrix-n
           (let ((inv-xfrm (invert-matrix xfrm))
                 (coord1 (make-array 3 :element-type 'double-float :initial-element 1d0)))
             (declare (type (simple-array double-float (3 3)) inv-xfrm)
                      (type (simple-array double-float (3)) coord1))
             (dotimes (i matrix-n-rows)
               (declare (type fixnum i))
               (setf (aref coord1 0) (coerce i 'double-float))
               (dotimes (j matrix-n-columns)
                 (declare (type fixnum i))
                 (setf (aref coord1 1) (coerce j 'double-float))
                 (multiple-value-bind (oldy oldx)
                     (values (+ (* (aref inv-xfrm 0 0) (aref coord1 0))
                                (* (aref inv-xfrm 0 1) (aref coord1 1))
                                (aref inv-xfrm 0 2))
                             (+ (* (aref inv-xfrm 1 0) (aref coord1 0))
                                (* (aref inv-xfrm 1 1) (aref coord1 1))
                                (aref inv-xfrm 1 2)))
                   (declare (type (double-float -1000000d0 1000000d0) oldy oldx))
                   (let ((oldy-round (truncate (+ oldy +epsilon+)))
                         (oldx-round (truncate (+ oldx +epsilon+))))
                     (when (and (< -1 oldy-round matrix-m-rows)
                                (< -1 oldx-round matrix-m-columns))
                       (setf (pixel matrix-n i j)
                             (pixel matrix-m oldy-round oldx-round))))))))))))

    (16-bit-gray-image
     (locally
         (declare (type 16-bit-gray-image matrix-m matrix-n))
       (with-image-bounds (matrix-m-rows matrix-m-columns)
           matrix-m
         (with-image-bounds (matrix-n-rows matrix-n-columns)
             matrix-n
           (let ((inv-xfrm (invert-matrix xfrm))
                 (coord1 (make-array 3 :element-type 'double-float :initial-element 1d0)))
             (declare (type (simple-array double-float (3 3)) inv-xfrm)
                      (type (simple-array double-float (3)) coord1))
             (dotimes (i matrix-n-rows)
               (declare (type fixnum i))
               (setf (aref coord1 0) (coerce i 'double-float))
               (dotimes (j matrix-n-columns)
                 (declare (type fixnum i))
                 (setf (aref coord1 1) (coerce j 'double-float))
                 (multiple-value-bind (oldy oldx)
                     (values (+ (* (aref inv-xfrm 0 0) (aref coord1 0))
                                (* (aref inv-xfrm 0 1) (aref coord1 1))
                                (aref inv-xfrm 0 2))
                             (+ (* (aref inv-xfrm 1 0) (aref coord1 0))
                                (* (aref inv-xfrm 1 1) (aref coord1 1))
                                (aref inv-xfrm 1 2)))
                   (declare (type (double-float -1000000d0 1000000d0) oldy oldx))
                   (let ((oldy-round (truncate (+ oldy +epsilon+)))
                         (oldx-round (truncate (+ oldx +epsilon+))))
                     (when (and (< -1 oldy-round matrix-m-rows)
                                (< -1 oldx-round matrix-m-columns))
                       (setf (pixel matrix-n i j)
                             (pixel matrix-m oldy-round oldx-round))))))))))))

    (8-bit-rgb-image
     (locally
         (declare (type 8-bit-rgb-image matrix-m matrix-n))
       (with-image-bounds (matrix-m-rows matrix-m-columns channels)
           matrix-m
         (with-image-bounds (matrix-n-rows matrix-n-columns)
             matrix-n
           (let ((inv-xfrm (invert-matrix xfrm))
                 (coord1 (make-array 3 :element-type 'double-float :initial-element 1d0)))
             (declare (type (simple-array double-float (3 3)) inv-xfrm)
                      (type (simple-array double-float (3)) coord1))
             (dotimes (i matrix-n-rows)
               (declare (type fixnum i))
               (setf (aref coord1 0) (coerce i 'double-float))
               (dotimes (j matrix-n-columns)
                 (declare (type fixnum i))
                 (setf (aref coord1 1) (coerce j 'double-float))
                 (multiple-value-bind (oldy oldx)
                     (values (+ (* (aref inv-xfrm 0 0) (aref coord1 0))
                                (* (aref inv-xfrm 0 1) (aref coord1 1))
                                (aref inv-xfrm 0 2))
                             (+ (* (aref inv-xfrm 1 0) (aref coord1 0))
                                (* (aref inv-xfrm 1 1) (aref coord1 1))
                                (aref inv-xfrm 1 2)))
                   (declare (type (double-float -1000000d0 1000000d0) oldy oldx))
                   (let ((oldy-round (truncate (+ oldy +epsilon+)))
                         (oldx-round (truncate (+ oldx +epsilon+))))
                     (declare (type (signed-byte 64) oldy-round oldx-round))
                     (when (and (< -1 oldy-round matrix-m-rows)
                                (< -1 oldx-round matrix-m-columns))
                       (setf (pixel matrix-n i j)
                             (pixel matrix-m oldy-round oldx-round))))))))))))
    
    (16-bit-rgb-image
     (locally
         (declare (type 16-bit-rgb-image matrix-m matrix-n))
       (with-image-bounds (matrix-m-rows matrix-m-columns channels)
           matrix-m
         (with-image-bounds (matrix-n-rows matrix-n-columns)
             matrix-n
           (let ((inv-xfrm (invert-matrix xfrm))
                 (coord1 (make-array 3 :element-type 'double-float :initial-element 1d0)))
             (declare (type (simple-array double-float (3 3)) inv-xfrm)
                      (type (simple-array double-float (3)) coord1))
             (dotimes (i matrix-n-rows)
               (declare (type fixnum i))
               (setf (aref coord1 0) (coerce i 'double-float))
               (dotimes (j matrix-n-columns)
                 (declare (type fixnum i))
                 (setf (aref coord1 1) (coerce j 'double-float))
                 (multiple-value-bind (oldy oldx)
                     (values (+ (* (aref inv-xfrm 0 0) (aref coord1 0))
                                (* (aref inv-xfrm 0 1) (aref coord1 1))
                                (aref inv-xfrm 0 2))
                             (+ (* (aref inv-xfrm 1 0) (aref coord1 0))
                                (* (aref inv-xfrm 1 1) (aref coord1 1))
                                (aref inv-xfrm 1 2)))
                   (declare (type (double-float -1000000d0 1000000d0) oldy oldx))
                   (let ((oldy-round (truncate (+ oldy +epsilon+)))
                         (oldx-round (truncate (+ oldx +epsilon+))))
                     (declare (type (signed-byte 64) oldy-round oldx-round))
                     (when (and (< -1 oldy-round matrix-m-rows)
                                (< -1 oldx-round matrix-m-columns))
                       (setf (pixel matrix-n i j)
                             (pixel matrix-m oldy-round oldx-round))))))))))))
    
    (8-bit-rgba-image
     (locally
         (declare (type 8-bit-rgba-image matrix-m matrix-n))
       (with-image-bounds (matrix-m-rows matrix-m-columns channels)
           matrix-m
         (with-image-bounds (matrix-n-rows matrix-n-columns)
             matrix-n
           (let ((inv-xfrm (invert-matrix xfrm))
                 (coord1 (make-array 3 :element-type 'double-float :initial-element 1d0)))
             (declare (type (simple-array double-float (3 3)) inv-xfrm)
                      (type (simple-array double-float (3)) coord1))
             (dotimes (i matrix-n-rows)
               (declare (type fixnum i))
               (setf (aref coord1 0) (coerce i 'double-float))
               (dotimes (j matrix-n-columns)
                 (declare (type fixnum i))
                 (setf (aref coord1 1) (coerce j 'double-float))
                 (multiple-value-bind (oldy oldx)
                     (values (+ (* (aref inv-xfrm 0 0) (aref coord1 0))
                                (* (aref inv-xfrm 0 1) (aref coord1 1))
                                (aref inv-xfrm 0 2))
                             (+ (* (aref inv-xfrm 1 0) (aref coord1 0))
                                (* (aref inv-xfrm 1 1) (aref coord1 1))
                                (aref inv-xfrm 1 2)))
                   (declare (type (double-float -1000000d0 1000000d0) oldy oldx))
                   (let ((oldy-round (truncate (+ oldy +epsilon+)))
                         (oldx-round (truncate (+ oldx +epsilon+))))
                     (declare (type (signed-byte 64) oldy-round oldx-round))
                     (when (and (< -1 oldy-round matrix-m-rows)
                                (< -1 oldx-round matrix-m-columns))
                       (setf (pixel matrix-n i j)
                             (pixel matrix-m oldy-round oldx-round))))))))))))
    
    (16-bit-rgba-image
     (locally
         (declare (type 16-bit-rgba-image matrix-m matrix-n))
       (with-image-bounds (matrix-m-rows matrix-m-columns channels)
           matrix-m
         (with-image-bounds (matrix-n-rows matrix-n-columns)
             matrix-n
           (let ((inv-xfrm (invert-matrix xfrm))
                 (coord1 (make-array 3 :element-type 'double-float :initial-element 1d0)))
             (declare (type (simple-array double-float (3 3)) inv-xfrm)
                      (type (simple-array double-float (3)) coord1))
             (dotimes (i matrix-n-rows)
               (declare (type fixnum i))
               (setf (aref coord1 0) (coerce i 'double-float))
               (dotimes (j matrix-n-columns)
                 (declare (type fixnum i))
                 (setf (aref coord1 1) (coerce j 'double-float))
                 (multiple-value-bind (oldy oldx)
                     (values (+ (* (aref inv-xfrm 0 0) (aref coord1 0))
                                (* (aref inv-xfrm 0 1) (aref coord1 1))
                                (aref inv-xfrm 0 2))
                             (+ (* (aref inv-xfrm 1 0) (aref coord1 0))
                                (* (aref inv-xfrm 1 1) (aref coord1 1))
                                (aref inv-xfrm 1 2)))
                   (declare (type (double-float -1000000d0 1000000d0) oldy oldx))
                   (let ((oldy-round (truncate (+ oldy +epsilon+)))
                         (oldx-round (truncate (+ oldx +epsilon+))))
                     (declare (type (signed-byte 64) oldy-round oldx-round))
                     (when (and (< -1 oldy-round matrix-m-rows)
                                (< -1 oldx-round matrix-m-columns))
                       (setf (pixel matrix-n i j)
                             (pixel matrix-m oldy-round oldx-round)))))))))))))
  matrix-n)


(defun %affine-transform-image (matrix-m matrix-n xfrm
                         &key
                         (interpolate :nearest-neighbor)
                         background)
  (let ((fit-function
         (let ((type (array-element-type matrix-n)))
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
    (with-image-bounds (matrix-m-rows matrix-m-columns channels)
        matrix-m
      (with-image-bounds (matrix-n-rows matrix-n-columns)
          matrix-n
        (let ((background (or background
                              (loop for i below (or channels 1) collect 0)))
              (inv-xfrm (invert-matrix xfrm))
              (coord1 (make-array 3 :element-type 'double-float :initial-element 1d0)))
          (dotimes (i matrix-n-rows)
            (setf (aref coord1 0) (coerce i 'double-float))
            (dotimes (j matrix-n-columns)
              (setf (aref coord1 1) (coerce j 'double-float))

              (multiple-value-bind (oldy oldx)
                  ;; since we don't need the full matrix multiply, based
                  ;; on what we know is in the affine transformation
                  ;; matrix, we can get away with fewer operations (Foley
                  ;; et al., 1996, p. 213)
                  (values (+ (* (aref inv-xfrm 0 0) (aref coord1 0))
                             (* (aref inv-xfrm 0 1) (aref coord1 1))
                             (aref inv-xfrm 0 2))
                          (+ (* (aref inv-xfrm 1 0) (aref coord1 0))
                             (* (aref inv-xfrm 1 1) (aref coord1 1))
                             (aref inv-xfrm 1 2)))

                (ecase interpolate
                  ((nil :nearest-neighbor)
                   (let ((oldy (floor (+ oldy +epsilon+)))
                         (oldx (floor (+ oldx +epsilon+))))
                     (if (and (< -1 oldy matrix-m-rows)
                              (< -1 oldx matrix-m-columns))
                         (setf (pixel matrix-n i j) (pixel matrix-m oldy oldx))
                         (setf (pixel matrix-n i j) (values-list background)))))
                  ;; quadratic is broken :(
                  #+nil (:quadratic
                   (if (and
                        (< most-negative-fixnum oldy most-positive-fixnum)
                        (< most-negative-fixnum oldx most-positive-fixnum))
                       (multiple-value-bind (l ry)
                           (truncate (+ oldy +epsilon+))
                         (multiple-value-bind (k rx)
                             (truncate (+ oldx +epsilon+))
                           (cond
                             ((and (< -1 l matrix-m-rows)
                                   (< -1 k matrix-m-columns))
                              (let ((l0 (max (1- l) 0))
                                    (l2 (min (1+ l) (1- matrix-m-rows)))
                                    (k0 (max (1- k) 0))
                                    (k2 (min (1+ k) (1- matrix-m-columns))))
                                (if channels
                                    (loop for channel below channels
                                       do 
                                       (setf (aref matrix-n i j channel)
                                             (max
                                              (min 
                                               (round
                                                (quadratic-interpolate
                                                 
                                                 (aref matrix-m l0 k0 channel)
                                                 (aref matrix-m l0 k channel)
                                                 (aref matrix-m l0 k2 channel)
                                                 (aref matrix-m l k0 channel)
                                                 (aref matrix-m l k channel)
                                                 (aref matrix-m l k2 channel)
                                                 (aref matrix-m l2 k0 channel)
                                                 (aref matrix-m l2 k channel)
                                                 (aref matrix-m l2 k2 channel)
                                                 ry rx))
                                               255)
                                              0)))
                                    (setf (pixel matrix-n i j)
                                          (max
                                           (min
                                            (round
                                             (quadratic-interpolate
                                              (aref matrix-m l0 k0)
                                              (aref matrix-m l0 k)
                                              (aref matrix-m l0 k2)
                                              (aref matrix-m l k0)
                                              (aref matrix-m l k)
                                              (aref matrix-m l k2)
                                              (aref matrix-m l2 k0)
                                              (aref matrix-m l2 k)
                                              (aref matrix-m l2 k2)
                                              ry rx))
                                            255)
                                           0)))))
                             (t
                              (setf (pixel matrix-n i j) (values-list background))))))
                       (setf (pixel matrix-n i j) (values-list background))))
                  ;; FIXME: bilinear interpolation has some weird edge
                  ;; effects. We should switch to nearest-neighbor at
                  ;; the edge maybe?
                  ((:bilinear :bi-linear)
                   (multiple-value-bind (l ry)
                       (floor (+ oldy +epsilon+))
                     (multiple-value-bind (k rx)
                         (floor (+ oldx +epsilon+))
                       (let ((l1 (1+ l))
                             (k1 (1+ k)))
                         (if channels
                             (loop for channel below channels
                                do 
                                (setf (aref matrix-n i j channel)
                                      (funcall fit-function
                                       (bilinear-interpolate
                                        (if (and (< -1 l matrix-m-rows)
                                                 (< -1 k matrix-m-columns))
                                            (aref matrix-m l k channel)
                                            (values-list background))
                                        (if (and (< -1 l matrix-m-rows)
                                                 (< -1 k1 matrix-m-columns))
                                            (aref matrix-m l k1 channel)
                                            (values-list background))
                                        (if (and (< -1 k matrix-m-columns)
                                                 (< -1 l1 matrix-m-rows))
                                            (aref matrix-m l1 k channel)
                                            (values-list background))
                                        (if (and (< -1 k1 matrix-m-columns)
                                                 (< -1 l1 matrix-m-rows))
                                            (aref matrix-m l1 k1 channel)
                                            (values-list background))
                                        ry rx))))
                             (setf (aref matrix-n i j)
                                   (funcall fit-function
                                    (bilinear-interpolate
                                     (if (and (< -1 l matrix-m-rows)
                                              (< -1 k matrix-m-columns))
                                         (aref matrix-m l k)
                                         (values-list background))
                                     (if (and (< -1 l matrix-m-rows)
                                              (< -1 k1 matrix-m-columns))
                                         (aref matrix-m l k1)
                                         (values-list background))
                                     (if (and (< -1 k matrix-m-columns)
                                              (< -1 l1 matrix-m-rows))
                                         (aref matrix-m l1 k)
                                         (values-list background))
                                     (if (and (< -1 k1 matrix-m-columns)
                                              (< -1 l1 matrix-m-rows))
                                         (aref matrix-m l1 k1)
                                         (values-list background))
                                     ry rx)))))))
                   )))))))))
  matrix-n)


(defun copy-transform (transform)
  (let ((new-transform (make-array '(3 3)
                                   :element-type (array-element-type transform))))
    (loop for i below 3
       do (loop for j below 3
             do (setf (aref new-transform i j) (pixel transform i j))))
    new-transform))

(defun compute-bounds (y1 x1 y2 x2 xfrm)
  "takes a region bound by x1 and x2 on the x-axis and y1 and y2 on
  the y-axis and returns the coordinates of the bounding rectangle
  after applying the affine transform xfrm"
  (multiple-value-bind (p1 q1)
      (transform-coord y1 x1 xfrm)
    (multiple-value-bind (p2 q2)
        (transform-coord y2 x2 xfrm)
      (multiple-value-bind (p3 q3)
          (transform-coord y2 x1 xfrm)
        (multiple-value-bind (p4 q4)
            (transform-coord y1 x2 xfrm)
          (values (min p1 p2 p3 p4) ;; y1'
                  (min q1 q2 q3 q4) ;; x1'
                  (max p1 p2 p3 p4) ;; y2'
                  (max q1 q2 q3 q4) ;; x2'
                  ))))))

(defun transform-image (matrix-m xfrm
                        &key u v y x
                        (transform-bounds t)
                        (interpolate :nearest-neighbor interpolate-supplied-p)
                        (background nil background-supplied-p))
  "applies the affine transform xfrm to the contents of matrix m and
   places the contents in n. The supported classes of interpolate
   are :bilinear and :nearest-neighbor. If interpolate parameter is
   not supplied, the default is :nearest-neighbor."
  (with-image-bounds (matrix-m-rows matrix-m-columns channels)
      matrix-m
    (let ((xfrm-shift (copy-transform xfrm)))
      (unless v (setf v (cons 0 matrix-m-rows)))
      (unless u (setf u (cons 0 matrix-m-columns)))
      (if transform-bounds
          (multiple-value-bind (y1 x1 y2 x2)
              (compute-bounds (car v) (car u) (cdr v) (cdr u) xfrm)
            (unless y (setf y (cons (floor (+ y1 +epsilon+))
                                    (ceiling (- y2 +epsilon+)))))
            (unless x (setf x (cons (floor (+ x1 +epsilon+))
                                    (ceiling (- x2 +epsilon+))))))
          (progn
            (unless y (setf y v))
            (unless x (setf x u))))
      (let ((matrix-n-rows (ceiling (- (cdr y) (car y))))
            (matrix-n-columns (ceiling (- (cdr x) (car x)))))
        (let ((matrix-n
               (make-array
                (list* matrix-n-rows matrix-n-columns (when channels (list channels)))
                :element-type (array-element-type matrix-m))))
          (let ((pre-shift1 (make-affine-transformation
                             :y-shift (car v) :x-shift (car u)))
                (pre-shift2 (make-affine-transformation
                             :y-scale (/ (- (cdr v) (car v)) matrix-m-rows)
                             :x-scale (/ (- (cdr u) (car u)) matrix-m-columns))))
            (setf xfrm-shift (matrix-multiply xfrm-shift
                                              (matrix-multiply pre-shift1 pre-shift2)))
            (let ((post-shift (make-affine-transformation
                               :y-shift (- (car y)) :x-shift (- (car x))))
                  (post-shift2 (make-affine-transformation
                                :y-scale (/ matrix-n-rows (- (cdr y) (car y))) 
                                :x-scale (/ matrix-n-columns (- (cdr x) (car x))))))
              (setf xfrm-shift
                    (matrix-multiply post-shift
                                     (matrix-multiply post-shift2 xfrm)))
              (if (and (or (and (typep matrix-m '8-bit-gray-image)
                                (typep matrix-n '8-bit-gray-image))
                           (and (typep matrix-m '16-bit-gray-image)
                                (typep matrix-n '16-bit-gray-image))

                           (and (typep matrix-m '8-bit-rgb-image)
                                (typep matrix-n '8-bit-rgb-image))
                           (and (typep matrix-m '16-bit-rgb-image)
                                (typep matrix-n '16-bit-rgb-image))
                           
                           (and (typep matrix-m '8-bit-rgba-image)
                                (typep matrix-n '8-bit-rgba-image))
                           (and (typep matrix-m '16-bit-rgba-image)
                                (typep matrix-n '16-bit-rgba-image)))
                       (not background-supplied-p)
                       (not interpolate-supplied-p))
                  (%fast-affine-transform-image matrix-m matrix-n xfrm-shift)
                  (apply #'%affine-transform-image matrix-m matrix-n xfrm-shift
                         (append
                          (when background-supplied-p
                            (list :background background))
                          (when interpolate-supplied-p
                            (list :interpolate interpolate))))))))))))

(defun split-around-zero (k &key integer)
  (let ((khalf (/ k 2.0d0)))
    (if integer
        (cons (floor (- khalf)) (ceiling khalf))
        (cons (+ (- khalf) 0.5d0) (+ khalf 0.5d0)))))

(defun resize-image (img y x &key interpolate)
  (with-image-bounds (oldy oldx channels)
      img
    (let ((yscale (/ y oldy)) (xscale (/ x oldx)))
      (let ((xfrm (make-affine-transformation :y-scale yscale :x-scale xscale)))
        (apply #'transform-image img xfrm
               (when interpolate `(:interpolate ,interpolate)))))))

(defun fit-image-into (img &key y-max x-max pad)
  (if (or y-max x-max)
      (with-image-bounds (oldy oldx channels)
          img
        (let ((scale (apply #'min 
                            (append (when y-max (list (/ y-max oldy)))
                                    (when x-max (list (/ x-max oldx)))))))
          (let ((xfrm (make-affine-transformation :y-scale scale :x-scale scale)))
            (if pad 
                (let ((y (* scale oldy)) (x (* scale oldx)))
                  (let ((maxdim (max y x)))
                    (let ((ypad (- maxdim y))
                          (xpad (- maxdim x)))
                      (let ((halfypad (/ ypad 2))
                            (halfxpad (/ xpad 2)))
                        (apply #'transform-image img xfrm
                               (when pad 
                                 (list :y (cons (floor (- halfypad))
                                                (floor (- y-max halfypad)))
                                       :x (cons (floor (- halfxpad))
                                                (floor (- x-max halfxpad))))))))))
                (transform-image img xfrm)))))
      img))

(defun rotate-image-around-center (img theta &key
                                   (transform-bounds t))
  (with-image-bounds (height width channels)
      img
    ;; shift the image center to 0, 0, rotate by theta and shift back
    (let ((pre-shift (make-affine-transformation :y-shift (- (/ height 2))
                                                 :x-shift (- (/ width 2))))
          (rotate (make-affine-transformation :theta theta))
          (post-shift (make-affine-transformation :y-shift (/ height 2)
                                                  :x-shift (/ width 2))))
      (let ((composed
             ;; transformation matricies must be multiplied in reverse order!
             (reduce #'matrix-multiply (reverse (list pre-shift rotate post-shift)))))
        (transform-image img composed :transform-bounds transform-bounds)))))

(defun get-locality-3x3 (img y x &key (spiral-order t))
  (with-image-bounds
   (height width) img
   (let ((height (- height 1))
         (width (- width 1)))
     (if spiral-order
         (list (pixel img y x)
               (if (= y 0) 0 (pixel img (- y 1) x))
               (if (or (= y 0) (= x width)) 0 (pixel img (- y 1) (+ x 1)))
               (if (= x width) 0 (pixel img y (+ x 1)))
               (if (or (= y height) (= x width)) 0 (pixel img (+ y 1) (+ x 1)))
               (if (= y height) 0 (pixel img (+ y 1) x))
               (if (or (= y height) (= x 0)) 0 (pixel img (+ y 1) (- x 1)))
               (if (= x 0) 0 (pixel img y (- x 1)))
               (if (or (= y 0) (= x 0)) 0 (pixel img (- y 1) (- x 1))))
       (list (if (or (= y 0) (= x 0)) 0
               (pixel img (- y 1) (- x 1)))
             (if (= y 0) 0
               (pixel img (- y 1) x))
             (if (or (= y 0) (= x width)) 0
               (pixel img (- y 1) (+ x 1)))
             (if (= x width) 0
               (pixel img y (+ x 1)))
             (if (or (= y height) (= x width)) 0
               (pixel img (+ y 1) (+ x 1)))
             (if (= y height) 0
               (pixel img (+ y 1) x))
             (if (or (= y height) (= x 0)) 0
               (pixel img (+ y 1) (- x 1)))
             (if (= x 0) 0
               (pixel img y (- x 1))))))))

(defun number-of-neighbors (img y x)
  (apply '+ (get-locality-3x3 img y x)))

(defun number-of-transitions (img y x)
  (let* ((locality (get-locality-3x3 img y x))
         (locality-pairs (make-pairs locality))
         (count-1-0 (count '(0 1) locality-pairs :test #'equalp))
         (count-0-1 (count '(1 0) locality-pairs :test #'equalp)))
    (max count-1-0 count-0-1)))

(defun thin-image-iteration (img)
  (with-image-bounds
   (height width) img
   (let ((result-img (make-1-bit-gray-image height width)))
     (loop for y from 1 below (1- height)
           do (loop for x from 1 below (1- width)
                    do (let* ((locality (get-locality-3x3 img y x :spiral-order nil))
                              (N (number-of-neighbors img y x))
                              (S (number-of-transitions img y x))
                              (condition1 (and (= (first locality) 1) (and (<= 2 N) (<= N 6)) (= S 1)
                                               (= (* (second locality) (fourth locality) (sixth locality)) 0)
                                               (= (* (eighth locality) (fourth locality) (sixth locality)) 0)))
                              (condition2 (and (= (first locality) 1) (and (<= 3 N) (<= N 6)) (= S 1)
                                               (= (* (second locality) (fourth locality) (sixth locality)) 0)
                                               (= (* (eighth locality) (fourth locality) (sixth locality)) 0))))
                         (setf (pixel result-img y x) (if condition1 0 (pixel img y x)))
                         (setf (pixel result-img y x) (if condition2 0 (pixel img y x))))))
     result-img)))

(defun thin-image (img)
  (let ((result-img (thin-image-iteration img))
        (prev-img nil))
    (loop for i from 1
          while (not (equalp prev-img result-img))
          do (progn
               (setf prev-img result-img)
               (setf result-img (thin-image-iteration result-img))))
    result-img))

