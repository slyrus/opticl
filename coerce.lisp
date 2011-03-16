
(in-package :opticl)

(defun mean (&rest numbers)
  (/ (apply #'+ numbers) (length numbers)))

(defgeneric coerce-image (image type &key &allow-other-keys)
  (:documentation "attempts to coerce a given image into the specified type."))

(defmethod coerce-image (image (type (eql '8-bit-gray-image)) &rest args)
  ;; work around ABCL etypecase bug
  (declare (ignore args))

  #-abcl
  (etypecase image
    (8-bit-gray-image image)
    (1-bit-gray-image 
     (with-image-bounds (y x) image
       (let* ((gray-image (make-8-bit-gray-image y x)))
         (do-pixels (i j) image
           (setf (pixel gray-image i j)
                 (if (plusp (pixel image i j)) 255 0)))
         gray-image)))
    (32-bit-gray-image
     (with-image-bounds (y x) image
       (let ((gray-image (make-8-bit-gray-image y x)))
         (do-pixels (i j)
             image
           (setf (pixel gray-image i j) (pixel image i j)))
         gray-image)))
    (fixnum-gray-image
     (with-image-bounds (y x) image
       (let ((gray-image (make-8-bit-gray-image y x)))
         (do-pixels (i j)
             image
           (setf (pixel gray-image i j) (pixel image i j)))
         gray-image)))
    ((or rgb-image rgba-image)
     (with-image-bounds (y x channels) image
       (let* ((type (array-element-type image))
              (gray-image (make-8-bit-gray-image y x)))
         (if (subtypep type 'integer)
             (do-pixels (i j)
                 image
               (multiple-value-bind (r g b)
                   (pixel image i j)
                 (setf (pixel gray-image i j)
                       (round (mean r g b)))))
             (do-pixels (i j)
                 image
               (multiple-value-bind (r g b)
                   (pixel image i j)
                 (setf (pixel gray-image i j)
                       (coerce (round (mean r g b)) type)))))
         gray-image))))

  ;; work around ABCL etypecase bug  
  #+abcl
  (cond
    ((typep image '8-bit-gray-image) image)
    
    ((typep image '1-bit-gray-image) 
     (with-image-bounds (y x)
         image
       (let* ((gray-image (make-8-bit-gray-image y x)))
         (do-pixels (i j) image
           (setf (pixel gray-image i j)
                 (if (plusp (pixel image i j)) 255 0)))
         gray-image)))

    ((typep image '32-bit-gray-image)
     (with-image-bounds (y x) image
       (let ((gray-image (make-8-bit-gray-image y x)))
         (do-pixels (i j)
             image
           (setf (pixel gray-image i j) (pixel image i j)))
         gray-image)))

    ((or (typep image 'rgb-image)
         (typep image 'rgba-image))
     (with-image-bounds (y x channels)
         image
       (let* ((type (array-element-type image))
              (gray-image (make-8-bit-gray-image y x)))
         (if (subtypep type 'integer)
             (do-pixels (i j)
                 image
               (multiple-value-bind (r g b)
                   (pixel image i j)
                 (setf (pixel gray-image i j)
                       (round (mean r g b)))))
             (do-pixels (i j)
                 image
               (multiple-value-bind (r g b)
                   (pixel image i j)
                 (setf (pixel gray-image i j)
                       (coerce (round (mean r g b)) type)))))
         gray-image)))))

(defmethod coerce-image (image (type (eql 'gray-image)) &key preserve-luminance &allow-other-keys)
  (etypecase image
    (gray-image image)
    ((or rgb-image rgba-image)
     (with-image-bounds (y x channels)
         image
       (let* ((type (array-element-type image))
              (gray-image (make-array (list y x) :element-type type)))
         (if preserve-luminance

             (if (subtypep type 'integer)
                 (do-pixels (i j)
                     image
                   (multiple-value-bind (r g b)
                       (pixel image i j)
                     (setf (pixel gray-image i j)
                           (round 
                            (+ (* r 0.2989)
                               (* g 0.5870)
                               (* b 0.1140))))))
                 (do-pixels (i j)
                     image
                   (multiple-value-bind (r g b)
                       (pixel image i j)
                     (setf (pixel gray-image i j)
                           (coerce (round 
                                    (+ (* r 0.2989)
                                       (* g 0.5870)
                                       (* b 0.1140))) type)))))
             (if (subtypep type 'integer)
                 (do-pixels (i j)
                     image
                   (multiple-value-bind (r g b)
                       (pixel image i j)
                     (setf (pixel gray-image i j)
                           (round (mean r g b)))))
                 (do-pixels (i j)
                     image
                   (multiple-value-bind (r g b)
                       (pixel image i j)
                     (setf (pixel gray-image i j)
                           (coerce (round (mean r g b)) type))))))
         gray-image)))))

(defmethod coerce-image (image (type (eql 'rgb-image)) &rest args)
  (declare (ignore args))
  (etypecase image
    (gray-image
     (with-image-bounds (y x channels)
         image
       (let* ((type (array-element-type image))
              (rgb-image (make-array (list y x 3) :element-type type)))
         (do-pixels (i j)
             image
           (let ((val (pixel image i j)))
             (setf (pixel rgb-image i j) 
                   (values val val val))))
         rgb-image)))
    (rgb-image image)
    (rgba-image
     (with-image-bounds (y x channels)
         image
       (let* ((type (array-element-type image))
              (rgb-image (make-array (list y x 3) :element-type type)))
         (do-pixels (i j)
             image
           (setf (pixel rgb-image i j) 
                 (pixel image i j)))
         rgb-image)))))

(defmethod coerce-image (image (type (eql 'rgba-image)) &rest args)
  (declare (ignore args))
  (etypecase image
    (gray-image
     (with-image-bounds (y x channels)
         image
       (let* ((type (array-element-type image))
              (rgba-image (make-array (list y x 4) :element-type type)))
         (do-pixels (i j)
             image
           (let ((val (pixel image i j)))
             (setf (pixel rgba-image i j) 
                   (values val val val 255))))
         rgba-image)))
    (rgb-image 
     (with-image-bounds (y x channels)
         image
       (let* ((type (array-element-type image))
              (rgba-image (make-array (list y x 4) :element-type type)))
         (do-pixels (i j)
             image
           (setf (pixel* rgba-image i j) 
                 (append (multiple-value-list (pixel image i j))
                         (list 255))))
         rgba-image)))
    (rgba-image image)))

;;;
;;; deprecated convert functions
(defun convert-image-to-8-bit-grayscale (image)
  (coerce image '8-bit-gray-image))

(defun convert-image-to-grayscale (image)
  (coerce-image image 'gray-image))

(defun convert-image-to-grayscale-luminance (image)
  (coerce-image image 'gray-image :preserve-luminance t))

(defun convert-image-to-rgb (image)
  (coerce-image image 'rgb-image))

(defun convert-image-to-rgba (image)
  (coerce-image image 'rgba-image))

