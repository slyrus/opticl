
(in-package :opticl)

;;
;; utility functions
(defun mean (&rest numbers)
  (/ (apply #'+ numbers) (length numbers)))

;;
;; macros that will use later

(defmacro rgb-to-hsv (image max-rgb-val)
  `(with-image-bounds (y x channels)
    ,image
    (let* ((hsv-image (make-hsv-image y x)))
      (declare (type hsv-image hsv-image))
      (do-pixels (i j)
          ,image
        (multiple-value-bind (r g b)
            (pixel ,image i j)
          (let ((r-prime (/ r ,max-rgb-val))
                (g-prime (/ g ,max-rgb-val))
                (b-prime (/ b ,max-rgb-val)))
            (let ((cmax (max r-prime g-prime b-prime))
                  (cmin (min r-prime g-prime b-prime)))
              (let ((delta (- cmax cmin)))
                (let ((h
                       (cond ((zerop delta)
                              0.0)
                             ((eql r-prime cmax)
                              (* 60 (mod  (/ (- g-prime b-prime)
                                             delta)
                                          6)))
                             ((eql g-prime cmax)
                              (* 60 (+ (/ (- b-prime r-prime)
                                          delta)
                                       2)))
                             ((eql b-prime cmax)
                              (* 60 (+ (/ (- r-prime g-prime)
                                          delta)
                                       4)))
                             (t (error "RGB -> HSV decoding error!"))))
                      (s (if (zerop cmax)
                             0.0
                             (/ delta cmax)))
                      (v cmax))
                  (setf (pixel hsv-image i j)
                        (make-hsv-pixel :hue h
                                                     :saturation s
                                                     :value v))))))))
      hsv-image)))

(defmacro hsv-to-rgb (rgb-image-type image max-rgb-val)
  (let ((ctor-function
         (read-from-string (format nil "make-~A" rgb-image-type))))
    `(with-image-bounds (y x)
      ,image
      (let* ((dest-image (,ctor-function y x)))
        (declare (type ,rgb-image-type dest-image))
        (do-pixels (i j)
            ,image
          (let ((hsv-pixel (pixel ,image i j)))
            (let ((h (hsv-pixel-hue hsv-pixel))
                  (s (hsv-pixel-saturation hsv-pixel))
                  (v (hsv-pixel-value hsv-pixel)))
              (let* ((c (* v s))
                     (x (* c (- 1 (abs (- (mod (/ h 60) 2) 1)))))
                     (m (- v c)))
                (multiple-value-bind (r-prime g-prime b-prime)
                    (cond ((and (>= h 0)
                                (< h 60))
                           (values c x 0))
                          ((and (>= h 60)
                                (< h 120))
                           (values x c 0))
                          ((and (>= h 120)
                                (< h 180))
                           (values 0 c x))
                          ((and (>= h 180)
                                (< h 240))
                           (values 0 x c))
                          ((and (>= h 240)
                                (< h 300))
                           (values 0 x c))
                          ((and (>= h 300)
                                (< h 360))
                           (values 0 x c)))
                  (let ((r (* (+ r-prime m) ,max-rgb-val))
                        (g (* (+ g-prime m) ,max-rgb-val))
                        (b (* (+ b-prime m) ,max-rgb-val)))
                    (setf (pixel dest-image i j)
                          (values (floor r) (floor g) (floor b)))))))))
        dest-image))))

(defgeneric coerce-image (image type &key &allow-other-keys)
  (:documentation "attempts to coerce a given image into the specified type."))

(defmethod coerce-image (image (type (eql '8-bit-gray-image)) &rest args)
  (declare (ignore args))

  (etypecase image
    (8-bit-gray-image image)
    (1-bit-gray-image 
     (with-image-bounds (y x) image
       (let* ((gray-image (make-8-bit-gray-image y x)))
         (declare (type 8-bit-gray-image gray-image)
                  (type 1-bit-gray-image image))
         (do-pixels (i j) image
           (setf (pixel gray-image i j)
                 (if (plusp (pixel image i j)) 255 0)))
         gray-image)))
    (32-bit-gray-image
     (with-image-bounds (y x) image
       (let ((gray-image (make-8-bit-gray-image y x)))
         (declare (type 8-bit-gray-image gray-image)
                  (type 32-bit-gray-image image))
         (do-pixels (i j)
             image
           (setf (pixel gray-image i j) (pixel image i j)))
         gray-image)))
    (fixnum-gray-image
     (with-image-bounds (y x) image
       (let ((gray-image (make-8-bit-gray-image y x)))
         (declare (type 8-bit-gray-image gray-image)
                  (type fixnum-gray-image image))
         (do-pixels (i j)
             image
           (setf (pixel gray-image i j) (pixel image i j)))
         gray-image)))
    (rgb-image
     (with-image-bounds (y x channels) image
       (let* ((type (array-element-type image))
              (gray-image (make-8-bit-gray-image y x)))
         (declare (type 8-bit-gray-image gray-image)
                  (type rgb-image image))
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
         gray-image)))
    (rgba-image
     (with-image-bounds (y x channels) image
       (let* ((type (array-element-type image))
              (gray-image (make-8-bit-gray-image y x)))
         (declare (type 8-bit-gray-image gray-image)
                  (type rgba-image image))
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
    (rgb-image
     (with-image-bounds (y x)
         image
       (let* ((type (array-element-type image))
              (gray-image (make-array (list y x) :element-type type)))
         (declare (type gray-image gray-image)
                  (type rgb-image image))
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
         gray-image)))
    (rgba-image
     ;; FIXME! I should do something with the alpha channel instead of
     ;; silently ignoring it!
     (with-image-bounds (y x)
         image
       (let* ((type (array-element-type image))
              (gray-image (make-array (list y x) :element-type type)))
         (declare (type gray-image gray-image)
                  (type rgba-image image))
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
     (locally
         (declare (type gray-image image))
       (with-image-bounds (y x)
           image
         (let* ((type (array-element-type image))
                (rgb-image (make-array (list y x 3) :element-type type)))
           (declare (type rgb-image rgb-image))
           (do-pixels (i j)
               image
             (let ((val (pixel image i j)))
               (setf (pixel rgb-image i j)
                     (values val val val))))
           rgb-image))))
    (rgb-image image)
    (rgba-image
     (locally
         (declare (type rgba-image image))
       (with-image-bounds (y x channels)
           image
         (let* ((type (array-element-type image))
                (rgb-image (make-array (list y x 3) :element-type type)))
           (do-pixels (i j)
               image
             (setf (pixel rgb-image i j)
                   (pixel image i j)))
           rgb-image))))))

(defmethod coerce-image (image (type (eql '8-bit-rgb-image)) &rest args)
  (declare (ignore args))
  (etypecase image
    ;; HACK ALERT!!
    ;; this needs to come before gray-image!!!
    (hsv-image
     (locally
         (declare (type hsv-image image))
       (hsv-to-rgb 8-bit-rgb-image image 255)))
    (gray-image
     (locally
         (declare (type gray-image image))
       (with-image-bounds (y x)
           image
         (let* ((8-bit-rgb-image (make-8-bit-rgb-image y x)))
           (declare (type 8-bit-rgb-image 8-bit-rgb-image))
           (do-pixels (i j)
               image
             (let ((val (pixel image i j)))
               (setf (pixel 8-bit-rgb-image i j)
                     (values val val val))))
           8-bit-rgb-image))))
    (8-bit-rgb-image image)
    (8-bit-rgba-image
     (locally
         (declare (type 8-bit-rgba-image image))
       (with-image-bounds (y x channels)
           image
         (let* ((8-bit-rgb-image (make-8-bit-rgb-image y x)))
           (declare (type 8-bit-rgb-image 8-bit-rgb-image))
           (do-pixels (i j)
               image
             (multiple-value-bind (r g b a)
                 (pixel image i j)
               (declare (ignore a))
               (setf (pixel 8-bit-rgb-image i j)
                     (values r g b))))
           8-bit-rgb-image))))
    (16-bit-rgb-image
     (locally
         (declare (type 16-bit-rgb-image image))
       (with-image-bounds (y x channels)
           image
         (let* ((8-bit-rgb-image (make-8-bit-rgb-image y x)))
           (declare (type 8-bit-rgb-image 8-bit-rgb-image))
           (do-pixels (i j)
               image
             (multiple-value-bind (r g b)
                 (pixel image i j)
               (setf (pixel 8-bit-rgb-image i j)
                     (values (ash r -8) (ash g -8) (ash b -8)))))
           8-bit-rgb-image))))
    (16-bit-rgba-image
     (locally
         (declare (type 16-bit-rgba-image image))
       (with-image-bounds (y x channels)
           image
         (let* ((8-bit-rgb-image (make-8-bit-rgb-image y x)))
           (declare (type 8-bit-rgb-image 8-bit-rgb-image))
           (do-pixels (i j)
               image
             (multiple-value-bind (r g b a)
                 (pixel image i j)
               (declare (ignore a))
               (setf (pixel 8-bit-rgb-image i j)
                     (values (ash r -8) (ash g -8) (ash b -8)))))
           8-bit-rgb-image))))
    ))

(defmethod coerce-image (image (type (eql '8-bit-rgba-image)) &rest args)
  (declare (ignore args))
  (etypecase image
    (gray-image
     (locally
         (declare (type gray-image image))
       (with-image-bounds (y x)
           image
         (let* ((type (array-element-type image))
                (rgba-image (make-array (list y x 4) :element-type type)))
           (declare (type rgba-image rgba-image))
           (do-pixels (i j)
               image
             (let ((val (pixel image i j)))
               (setf (pixel rgba-image i j)
                     (values val val val 255))))
           rgba-image))))
    (8-bit-rgb-image
     (locally
         (declare (type 8-bit-rgb-image image))
       (with-image-bounds (y x channels)
           image
         (let* ((8-bit-rgba-image (make-8-bit-rgba-image y x)))
           (declare (type 8-bit-rgba-image 8-bit-rgba-image))
           (do-pixels (i j)
               image
             (multiple-value-bind (r g b)
                 (pixel image i j)
               (setf (pixel 8-bit-rgba-image i j)
                     (values r g b #xff))))
           8-bit-rgba-image))))
    (8-bit-rgba-image image)
    (16-bit-rgb-image
     (locally
         (declare (type 16-bit-rgb-image image))
       (with-image-bounds (y x channels)
           image
         (let* ((8-bit-rgba-image (make-8-bit-rgba-image y x)))
           (declare (type 8-bit-rgba-image 8-bit-rgba-image))
           (do-pixels (i j)
               image
             (multiple-value-bind (r g b)
                 (pixel image i j)
               (setf (pixel 8-bit-rgba-image i j)
                     (values (ash r -8) (ash g -8) (ash b -8) #xff))))
           8-bit-rgba-image))))
    (16-bit-rgba-image
     (locally
         (declare (type 16-bit-rgba-image image))
       (with-image-bounds (y x channels)
           image
         (let* ((8-bit-rgba-image (make-8-bit-rgba-image y x)))
           (declare (type 8-bit-rgba-image 8-bit-rgba-image))
           (do-pixels (i j)
               image
             (multiple-value-bind (r g b a)
                 (pixel image i j)
               (setf (pixel 8-bit-rgba-image i j)
                     (values (ash r -8) (ash g -8) (ash b -8) (ash a -8)))))
           8-bit-rgba-image))))
    (rgb-image
     (locally
         (declare (type rgb-image image))
       (with-image-bounds (y x channels)
           image
         (let* ((type (array-element-type image))
                (rgba-image (make-array (list y x 4) :element-type type)))
           (declare (type rgba-image rgba-image))
           (do-pixels (i j)
               image
             (setf (pixel* rgba-image i j)
                   (append (multiple-value-list (pixel image i j))
                           (list 255))))
           rgba-image))))
    (rgba-image image)))

(defmethod coerce-image (image (type (eql '16-bit-rgb-image)) &rest args)
  (declare (ignore args))
  (etypecase image
    ;; HACK ALERT!!
    ;; this needs to come before gray-image!!!
    (hsv-image
     (locally
         (declare (type hsv-image image))
       (hsv-to-rgb 16-bit-rgb-image image #xFFFF)))
    (gray-image
     (locally
         (declare (type gray-image image))
       (with-image-bounds (y x)
           image
         (let* ((16-bit-rgb-image (make-16-bit-rgb-image y x)))
           (declare (type 16-bit-rgb-image 16-bit-rgb-image))
           (do-pixels (i j)
               image
             (let ((val (ash (pixel image i j) 8)))
               (setf (pixel 16-bit-rgb-image i j)
                     (values val val val))))
           16-bit-rgb-image))))
    (8-bit-rgb-image
     (locally
         (declare (type 8-bit-rgb-image image))
       (with-image-bounds (y x channels)
           image
         (let* ((16-bit-rgb-image (make-16-bit-rgb-image y x)))
           (declare (type 16-bit-rgb-image 16-bit-rgb-image))
           (do-pixels (i j)
               image
             (multiple-value-bind (r g b)
                 (pixel image i j)
               (setf (pixel 16-bit-rgb-image i j)
                     (values (ash r 8) (ash g 8) (ash b 8)))))
           16-bit-rgb-image))))
    (8-bit-rgba-image
     (locally
         (declare (type 8-bit-rgba-image image))
       (with-image-bounds (y x channels)
           image
         (let* ((16-bit-rgb-image (make-16-bit-rgb-image y x)))
           (declare (type 16-bit-rgb-image 16-bit-rgb-image))
           (do-pixels (i j)
               image
             (multiple-value-bind (r g b)
                 (pixel image i j)
               (setf (pixel 16-bit-rgb-image i j)
                     (values (ash r 8) (ash g 8) (ash b 8)))))
           16-bit-rgb-image))))
    (16-bit-rgba-image
     (locally
         (declare (type 16-bit-rgba-image image))
       (with-image-bounds (y x channels)
           image
         (let* ((16-bit-rgb-image (make-16-bit-rgb-image y x)))
           (declare (type 16-bit-rgb-image 16-bit-rgb-image))
           (do-pixels (i j)
               image
             (multiple-value-bind (r g b a)
                 (pixel image i j)
               (declare (ignore a))
               (setf (pixel 16-bit-rgb-image i j)
                     (values r g b))))
           16-bit-rgb-image))))))

(defmethod coerce-image (image (type (eql '16-bit-rgba-image)) &rest args)
  (declare (ignore args))
  (etypecase image
    (gray-image
     (locally
         (declare (type gray-image image))
       (with-image-bounds (y x)
           image
         (let* ((16-bit-rgba-image (make-16-bit-rgba-image y x)))
           (declare (type 16-bit-rgba-image 16-bit-rgba-image))
           (do-pixels (i j)
               image
             (let ((val (ash (pixel image i j) 8)))
               (setf (pixel 16-bit-rgba-image i j)
                     (values val val val #xffff))))
           16-bit-rgba-image))))
    (8-bit-rgb-image
     (locally
         (declare (type 8-bit-rgb-image image))
       (with-image-bounds (y x channels)
           image
         (let* ((16-bit-rgba-image (make-16-bit-rgba-image y x)))
           (declare (type 16-bit-rgba-image 16-bit-rgba-image))
           (do-pixels (i j)
               image
             (multiple-value-bind (r g b)
                 (pixel image i j)
               (setf (pixel 16-bit-rgba-image i j)
                     (values (ash r 8) (ash g 8) (ash b 8) #xffff))))
           16-bit-rgba-image))))
    (8-bit-rgba-image
     (locally
         (declare (type 8-bit-rgba-image image))
       (with-image-bounds (y x channels)
           image
         (let* ((16-bit-rgba-image (make-16-bit-rgba-image y x)))
           (declare (type 16-bit-rgba-image 16-bit-rgba-image))
           (do-pixels (i j)
               image
             (multiple-value-bind (r g b a)
                 (pixel image i j)
               (setf (pixel 16-bit-rgba-image i j)
                     (values (ash r 8) (ash g 8) (ash b 8) (ash a 8)))))
           16-bit-rgba-image))))
    (16-bit-rgb-image
     (locally
         (declare (type 16-bit-rgb-image image))
       (with-image-bounds (y x channels)
           image
         (let* ((16-bit-rgba-image (make-16-bit-rgba-image y x)))
           (declare (type 16-bit-rgba-image 16-bit-rgba-image))
           (do-pixels (i j)
               image
             (multiple-value-bind (r g b)
                 (pixel image i j)
               (setf (pixel 16-bit-rgba-image i j)
                     (values r g b #xffff))))
           16-bit-rgba-image))))
    (16-bit-rgba-image image)))

(defmethod coerce-image (image (type (eql 'rgba-image)) &rest args)
  (apply #'coerce-image image '8-bit-rgba-image args))

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

(defmethod coerce-image (image (type (eql 'hsv-image)) &rest args)
  (declare (ignore args))
  (etypecase image
    (8-bit-rgb-image
     (locally
         (declare (type 8-bit-rgb-image image))
       (rgb-to-hsv image (float #xFF))))
    (16-bit-rgb-image
     (locally
         (declare (type 16-bit-rgb-image image))
       (rgb-to-hsv image (float #xFFFF))))))
