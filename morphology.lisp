
(in-package :opticl)

(defun 4-neighbors (img i j)
  "Returns four values, each value is either a list containing
the coordinates of a 4-neighbor of (i,j) in img or nil if the
neighbor would be outside of the img. The order of the values
is top, left, bottom, right."
  (declare (type fixnum i j))
  (with-image-bounds (height width)
      img
    (values (when (> i 0) (list (1- i) j))             ; top
            (when (> j 0) (list i (1- j)))             ; left 
            (when (< i (1- height)) (list (1+ i) j))   ; bottom
            (when (< j (1- width)) (list i (1+ j)))     ; right
            )))

(defun 8-neighbors (img i j)
  "Returns eight values, each value is either a list containing
the coordinates of an 8-neighbor of (i,j) in img or nil if the
neighbor would be outside of the img. The order of the values
is top, left, bottom, right."
  (declare (type fixnum i j))
  (with-image-bounds (height width)
      img
    (values (when (and (> i 0) (> j 0)) (list (1- i) (1- j))) ; top-left
            (when (> j 0) (list i (1- j)))                    ; left
            (when (and (< i (1- height)) (> j 0)) (list (1+ i) (1- j))) ; bottom-left
            (when (< i (1- height)) (list (1+ i) j)) ; bottom
            (when (and (< i (1- height)) (< j (1- width))) (list (1+ i) (1+ j))) ; bottom-right
            (when (< j (1- width)) (list i (1+ j))) ; right
            (when (and (> i 0) (< j (1- width))) (list (1- i) (1+ j))) ; top-right
            (when (> i 0) (list (1- i) j)) ; top
            )))


(defmacro multiple-value-list-remove-nulls (values)
  `(remove-if #'null (multiple-value-list ,values)))

(defun label-components (img &key (neighbor-function #'4-neighbors))
  "Returns an array containing labels of the connected
components of matrix. The default neighbor-function is
4-neighbors."
  (with-image-bounds (height width)
      img
    (let ((label-array (make-array (list height width)
                                   :element-type 'fixnum
                                   :initial-element 0))
          (stack)
          (label-value 0))
      (dotimes (i height)
        (dotimes (j width)
          (when (= 0 (aref label-array i j))
            (let ((current-label-value (multiple-value-list (pixel img i j))))
              (incf label-value)
              (setf (aref label-array i j) label-value)
              (mapcar (lambda (p)
                        (destructuring-bind (ni nj) p
                          (when (equalp current-label-value
                                        (multiple-value-list
                                         (pixel img ni nj)))
                            (push p stack)
                            (setf (aref label-array ni nj) label-value))))
                      (multiple-value-list-remove-nulls
                       (funcall neighbor-function img i j)))
              ;; now we walk through the list....
              (do ((k (pop stack) (pop stack)))
                  ((null k))
                (mapcar (lambda (p)
                          (destructuring-bind (ni nj) p
                            (when (and (equalp current-label-value
                                               (multiple-value-list
                                                (pixel img ni nj)))
                                       (= 0 (aref label-array ni nj)))
                              (push p stack)
                              (setf (aref label-array ni nj) label-value))))
                        (multiple-value-list-remove-nulls
                         (funcall neighbor-function img (car k) (cadr k)))))))))
      (map-array #'1- label-array))))

(defun make-fit-function (image)
  (let ((type (array-element-type image)))
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
      (t #'identity))))

(defun morphological-op (u v f)
  (let ((fit-fn (make-fit-function u)))
    (with-image-bounds (uy ux channels) u
      (with-image-bounds (vy vx) v
        (let ((zy (+ uy vy (- 1)))
              (zx (+ ux vx (- 1))))
          (let ((z (make-array (apply #'list zy zx
                                      (when channels (list channels)))
                               :element-type (array-element-type u))))
            (do-pixels (i j) z
              (let ((ustartr (max 0 (- i vy -1)))
                    (uendr (min (- uy 1) i))
                    (vstartr (- vy (max (- vy i) 1))))
                (let ((ustartc (max 0 (- j vx -1)))
                      (uendc (min (- ux 1) j))
                      (vstartc (- vx (max (- vx j) 1)))
                      acc)
                  (do ((urow ustartr (1+ urow))
                       (vrow vstartr (1- vrow)))
                      ((> urow uendr))
                    (do ((ucol ustartc (1+ ucol))
                         (vcol vstartc (1- vcol)))
                        ((> ucol uendc))
                      (setf acc (funcall f acc (pixel* u urow ucol) (pixel* v vrow vcol)))))
                  (setf (pixel* z i j) (map 'list fit-fn acc)))))
            z))))))

(defun dilate (u v)
  (morphological-op u v #'(lambda (acc upixel vpixel)
                            (let ((v (map 'list #'* upixel vpixel)))
                              (cond ((null acc) v)
                                    (t (map 'list #'max acc v)))))))

(defun erode (u v)
  (morphological-op u v #'(lambda (acc upixel vpixel)
                            (let ((v (map 'list #'* upixel vpixel)))
                              (cond ((null acc) v)
                                    (t (map 'list #'min acc v)))))))

(defun guo-hall-thinning (img &key (threshold 25))
  (etypecase img
    (8-bit-gray-image
     (loop for i from 0 below (array-total-size img)
	   do (setf (row-major-aref img i)
		    (if (>= (row-major-aref img i) threshold) 1 0)))
     ;; perform transform
     (destructuring-bind (h w) (array-dimensions img)
       (let ((map (make-1-bit-gray-image h w
					 :initial-element 0)))
	 (labels ((at (i y x)
		    (and (pixel-in-bounds i y x)
			 (plusp (pixel i y x))))
		  (bi (v)
		    (if v 1 0))
		  (cycle (img evenp)
		    (loop for y fixnum from 0 below h do
		      (loop for x fixnum from 0 below w
			    for p2 = (at img (1- y) x)
			    for p3 = (at img (1- y) (1+ x))
			    for p4 = (at img y (1+ x))
			    for p5 = (at img (1+ y) (1+ x))
			    for p6 = (at img (1+ y) x)
			    for p7 = (at img (1+ y) (1- x))
			    for p8 = (at img y (1- x))
			    for p9 = (at img (1- y) (1- x))
			    for c = (+ (bi (and (not p2) (or p3 p4)))
				       (bi (and (not p4) (or p5 p6)))
				       (bi (and (not p6) (or p7 p8)))
				       (bi (and (not p8) (or p9 p2))))
			    for n1 = (+ (bi (or p9 p2)) (bi (or p3 p4))
					(bi (or p5 p6)) (bi (or p7 p8)))
			    for n2 = (+ (bi (or p2 p3)) (bi (or p4 p5))
					(bi (or p6 p7)) (bi (or p8 p9)))
			    for n = (min n1 n2)
			    for m = (if evenp
					(and (or p2 p3 (not p5)) p4)
					(and (or p6 p7 (not p9)) p8))
			    when (and (= c 1) (<= 2 n 3) (not m))
			      do (setf (pixel map y x) 1)))
		    (loop for i from 0 below (array-total-size img)
			  do (setf (row-major-aref img i)
				   (logand (row-major-aref img i)
					   (if (plusp (row-major-aref map i)) 0 1))))))
	   (loop with prev
		 until (equalp img prev) do
		   (setf prev (copy-image img))
		   (cycle img t)
		   (cycle img nil)))))
     ;; back to grayscale
     (loop for i from 0 below (array-total-size img)
	   do (setf (row-major-aref img i) (* (row-major-aref img i) 255)))
     img)))
