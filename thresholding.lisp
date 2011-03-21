;;; The below code is in the public domain.

(in-package :opticl)

(defun threshold-image (image threshold)
  (declare (type gray-image image)
           (type number threshold))
  (with-image-bounds (height width) image
    (let ((binary-image (make-1-bit-gray-image height width
                                               :initial-element 0)))
      (declare (type gray-image image))
      (do-pixels (i j) image
        (when (>= (pixel image i j) threshold)
          (setf (pixel binary-image i j) 1)))
      (the 1-bit-gray-image binary-image))))

(defconstant +8-bit-values-count+ 256)
(defconstant +8-bit-max-value+ 255)

(defun compute-histogram (image)
  (declare (type 8-bit-gray-image image))
  (let ((histogram (make-array +8-bit-values-count+
                               :element-type 'single-float
                               :initial-element 0f0)))
    (do-pixels (i j) image
      (incf (aref histogram (pixel image i j))))
    (with-image-bounds (height width) image
      (let ((size (* height width)))
        (loop for i to +8-bit-max-value+
           do (setf (aref histogram i)
                    (/ (aref histogram i) size)))))
    histogram))

(defun threshold-image-with-min-error (image)
  (declare (type 8-bit-gray-image image)
           (optimize (debug 3)))
  (let* ((threshold (truncate (/ +8-bit-max-value+ 2)))
         (prev-threshold threshold)
         (histogram (compute-histogram image)))
    (macrolet ((compute-upto-threshold (expr)
                 `(loop for i to threshold
                     summing ,expr))
               (compute-from-threshold (expr)
                 `(loop for i from (1+ threshold) to +8-bit-max-value+
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
                   (log-apriori-1 (log apriori-1 10))
                   (log-apriori-2 (log apriori-2 10))
                   (log-deviation-1 (log deviation-1 10))
                   (log-deviation-2 (log deviation-2 10))
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
              (setf prev-threshold threshold
                    threshold (truncate (if (< discriminant 0)
                                            (abs (random +8-bit-values-count+))
                                            (/ (+ (- b) (sqrt discriminant))
                                               (* 2 a))))))
         while (/= threshold prev-threshold)))
    (threshold-image image threshold)))
