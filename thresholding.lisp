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
