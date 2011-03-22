;;; Originally written by Ivan Chernetsky in 2011.
;;; The below code is in the public domain.

(in-package :opticl)

(defun threshold-image (image threshold)
  "Performs simple thresholding of grayscale image and returns a
binarized image of type 1-bit-gray-image. Before thresholding
threshold is coerced to type of image's elements.

An error of type type-error is signaled if image is not of
gray-image type."
  (etypecase image
    (gray-image
     (with-image-bounds (height width) image
       (let ((binary-image (make-1-bit-gray-image height width :initial-element 0))
             (threshold (coerce threshold (array-element-type image))))
         (declare (type gray-image image)
                  (type 1-bit-gray-image binary-image))
         (do-pixels (i j) image
           (when (>= (pixel image i j) threshold)
             (setf (pixel binary-image i j) 1)))
         (the 1-bit-gray-image binary-image))))))

(defconstant +8-bit-values-count+ 256
  "Number of different values an element of 8-bit-gray-image
image can be of.")

(defconstant +8-bit-max-value+ 255
  "Maximum value an element of 8-bit-gray-image image can be.")

(defun compute-histogram (image)
  "Computes a normalized histogram of 8-bit-gray-image image,
i.e. an estimate of the probability density function of gray
levels, and returns it."
  (declare (type 8-bit-gray-image image))
  (let ((histogram (make-array +8-bit-values-count+
                               :element-type 'double-float
                               :initial-element 0d0)))
    (do-pixels (i j) image
      (incf (aref histogram (pixel image i j))))
    (with-image-bounds (height width) image
      (let ((size (* height width)))
        (loop for i below +8-bit-values-count+
           do (setf (aref histogram i)
                    (/ (aref histogram i) size)))))
    histogram))

(defun threshold-image-with-min-error (image)
  "Binarize 8-bit-gray-image image with an automatically guessed
threshold. Returns 1-bit-gray-image image as a result.

For further details, please refer 'Minumum error thresholding' by
J. Kittler and J. Illingworth."
  (declare (optimize (debug 3)))
  (etypecase image
    (8-bit-gray-image
     (let* ((threshold (coerce (/ +8-bit-max-value+ 2) 'double-float))
            (prev-threshold threshold)
            (loop-threshold (truncate threshold))
            (histogram (compute-histogram image)))
       (declare (type 8-bit-gray-image image)
                (type double-float threshold prev-threshold))
       (macrolet ((compute-upto-threshold (expr)
                    `(loop for i to loop-threshold
                        summing ,expr))
                  (compute-from-threshold (expr)
                    `(loop for i from (1+ loop-threshold) to +8-bit-max-value+
                        summing ,expr)))
         (loop
            do (let* ((apriori-1 (compute-upto-threshold (aref histogram i)))
                      (apriori-2 (compute-from-threshold (aref histogram i)))
                      (mean-1 (/ (compute-upto-threshold (* (aref histogram i) i))
                                 apriori-1))
                      (mean-2 (/ (compute-from-threshold (* (aref histogram i) i))
                                 apriori-2))
                      (deviation-1 (/ (compute-upto-threshold (* (* (- i mean-1)
                                                                    (- i mean-1))
                                                                 (aref histogram i)))
                                      apriori-1))
                      (deviation-2 (/ (compute-from-threshold (* (* (- i mean-2)
                                                                    (- i mean-2))
                                                                 (aref histogram i)))
                                      apriori-2))
                      (variance-1 (* deviation-1 deviation-1))
                      (variance-2 (* deviation-2 deviation-2))
                      (log-apriori-1 (log apriori-1))
                      (log-apriori-2 (log apriori-2))
                      (log-deviation-1 (log deviation-1))
                      (log-deviation-2 (log deviation-2))
                      (mean-squared-1 (* mean-1 mean-1))
                      (mean-squared-2 (* mean-2 mean-2))
                      (a (- (/ 1 variance-1)
                            (/ 1 variance-2)))
                      (b (* (- (/ mean-1 variance-1)
                               (/ mean-2 variance-2))
                            -2))
                      (c (+ (/ mean-squared-1 variance-1)
                            (- (/ mean-squared-2 variance-2))
                            (* 2 (- log-deviation-1 log-deviation-2))
                            (* -2 (- log-apriori-1 log-apriori-2))))
                      (discriminant (- (* b b)
                                       (* 4 a c))))
                 (format t "~% a: ~f~% b: ~f~% c: ~f~%" a b c)
                 (format t "~% discriminant: ~f~%" discriminant)
                 (setf prev-threshold threshold
                       threshold (if (< discriminant 0)
                                     (coerce (abs (random +8-bit-values-count+))
                                             'double-float)
                                     (/ (- (- b) (sqrt discriminant))
                                        (* 2 a)))
                       loop-threshold (min (truncate threshold)
                                           +8-bit-max-value+))
                 (format t "~% reached! new threshold: ~f~%" threshold))
            while (/= threshold prev-threshold)))
       (threshold-image image threshold)))))
