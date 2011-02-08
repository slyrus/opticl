
(in-package :opticl)

(deftype affine-transformation ()
  `(simple-array single-float (3 3)))

(defmethod make-affine-transformation* (&key
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

