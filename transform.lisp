
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
                                   (y-shift 0d0)
                                   (x-shift 0d0)
                                   (theta 0d0)
                                   (y-scale 1d0)
                                   (x-scale 1d0)
                                   (y-shear 0d0)
                                   (x-shear 0d0))
  (let ((xfrm (make-array (list 3 3)
                          :element-type 'double-float
                          :initial-element 0d0)))
    (setf (aref xfrm 0 0) (- (* (cos theta) y-scale)
                             (* (sin theta) x-scale x-shear)))
    (setf (aref xfrm 0 1) (- (* (cos theta) y-scale y-shear)
                             (* (sin theta) x-scale)))
    (setf (aref xfrm 0 2) (coerce y-shift 'double-float))

    (setf (aref xfrm 1 0) (+ (* (sin theta) y-scale)
                             (* (cos theta) x-scale x-shear)))
    (setf (aref xfrm 1 1) (+ (* (sin theta) y-scale y-shear)
                             (* (cos theta) x-scale)))
    (setf (aref xfrm 1 2) (coerce x-shift 'double-float))
  
    (setf (aref xfrm 2 0) 0d0)
    (setf (aref xfrm 2 1) 0d0)
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

(defun %transform-coord (coord xfrm)
  "applies the affine transformation xfrm to the point {x,y} and
  returns the position of the point after applying the transformation"
  (post-multiply-by-column-vector xfrm coord))

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

(defun %transform-image (matrix-m matrix-n xfrm
                         &key
                         (interpolation :nearest-neighbor)
                         background)
  (declare (optimize (debug 3)))
  (with-image-bounds (matrix-m-rows matrix-m-columns channels)
      matrix-m
    (with-image-bounds (matrix-n-rows matrix-n-columns)
        matrix-n
      (let ((background (or background
                            (loop for i below (or channels 1) collect 0)))
            (inv-xfrm (invert-matrix xfrm))
            (coord1 (make-array 3 :element-type 'double-float)))
        #+nil (print inv-xfrm)
        (setf (aref coord1 2) 1d0)
        (dotimes (i matrix-n-rows)
          (setf (aref coord1 0) (coerce i 'double-float))
          (dotimes (j matrix-n-columns)
            (setf (aref coord1 1) (coerce j 'double-float))

            (multiple-value-bind (oldy oldx)
                ;; faster way
                ;;
                ;; since we don't need the full matrix multiply, based
                ;; on what we know is in the affine transformation
                ;; matrix, we can get away with fewer operations (Foley
                ;; et al., 1996, p. 213)
                (values (+ (* (aref coord1 0) (aref inv-xfrm 0 0))
                           (* (aref coord1 1) (aref inv-xfrm 0 1))
                           (aref inv-xfrm 0 2))
                        (+ (* (aref coord1 0) (aref inv-xfrm 1 0))
                           (* (aref coord1 1) (aref inv-xfrm 1 1))
                           (aref inv-xfrm 1 2)))
              ;; slower way
              #+nil (transform-coord (aref coord1 0)
                                     (aref coord1 1)
                                     inv-xfrm)
              
              (case interpolation
                ((nil :nearest-neighbor)
                 (let ((oldy (round (- oldy +epsilon+)))
                       (oldx (round (- oldx +epsilon+))))
                   #+nil (print (list oldy oldx))
                   (if (and (< -1 oldy matrix-m-rows)
                            (< -1 oldx matrix-m-columns))
                       (setf (pixel matrix-n i j) (pixel matrix-m oldy oldx))
                       (setf (pixel matrix-n i j) (values-list background)))))
                )))))))
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

(defun transform-image (matrix-m matrix-n xfrm
                             &key u v y x
                             (interpolation :nearest-neighbor interpolation-supplied-p)
                             (background nil background-supplied-p))
  "applies the affine transform xfrm to the contents of matrix m and
   places the contents in n. The default supported classes of
   interpolation are :quadratic, :bilinear and :nearest-neighbor. If
   no interpolation is supplied, the default is :nearest-neighbor."
  (with-image-bounds (matrix-m-rows matrix-m-columns)
      matrix-m
    (with-image-bounds (matrix-n-rows matrix-n-columns)
        matrix-n
      (let ((xfrm-shift (copy-transform xfrm)))
        (unless v
          (setf v (cons 0 matrix-m-rows)))
        (unless u
          (setf u (cons 0 matrix-m-columns)))
        (multiple-value-bind (y1 x1 y2 x2)
            (compute-bounds (car v) (car u) (cdr v) (cdr u) xfrm)
          #+nil (print (list y1 x1 y2 x2))
          (unless y
            (setf y (cons (floor (+ y1 +epsilon+))
                          (floor (- y2 +epsilon+)))))
          (unless x
            (setf x (cons (floor (+ x1 +epsilon+))
                          (floor (- x2 +epsilon+)))))
          #+nil (print (list y x)))
                
        ;; Need to rework math to do the right thing here!

        (let ((pre-shift1 (make-affine-transformation
                           :y-shift (car v) :x-shift (car u)))
              (pre-shift2 (make-affine-transformation
                           :y-scale (/ (- (cdr v) (car v)) matrix-m-rows)
                           :x-scale (/ (- (cdr u) (car u)) matrix-m-columns))))
          (setf xfrm-shift (matrix-multiply xfrm-shift (matrix-multiply pre-shift1 pre-shift2)))
          (let ((post-shift (make-affine-transformation
                             :y-shift (- (1+ (car y))) :x-shift (- (1+ (car x)))))
                (post-shift2 (make-affine-transformation
                              :y-scale (/ matrix-n-rows (- (cdr y) (car y))) 
                              :x-scale (/ matrix-n-columns (- (cdr x) (car x))))))
            (setf xfrm-shift (matrix-multiply post-shift (matrix-multiply post-shift2 xfrm-shift)))
            (apply #'%transform-image matrix-m matrix-n xfrm
                   (append
                    (when background-supplied-p (list :background background))
                    (when interpolation-supplied-p (list :interpolation interpolation))))))))))

