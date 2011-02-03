
(in-package :opticl)

(defconstant +ncomp-gray+ 1)
(defconstant +ncomp-rgb+ 3)
(defconstant +ncomp-rgba+ 4)

(defparameter *rgb-sampling* '((1 1)(1 1)(1 1)))
(defparameter *rgba-sampling* '((1 1)(1 1)(1 1)(1 1)))

;;;
;;; Reading JPEG files
(defun read-jpeg-stream (stream)
  (multiple-value-bind (buffer height width ncomp)
      (jpeg:decode-stream stream)
    (cond
      ((= ncomp +ncomp-rgb+)
       (let ((image (make-8-bit-rgb-image height width)))
               (loop for i below height
                  do 
                    (loop for j below width
                       do 
                         (let ((pixoff (* +ncomp-rgb+ (+ (* i width) j))))
                           (setf (8-bit-rgb-pixel image i j)
                                 (values (aref buffer (+ 2 pixoff))
                                         (aref buffer (+ 1 pixoff))
                                         (aref buffer  pixoff))))))
               image))
      ((= ncomp 1)
       (error "grayscale JPEGs not yet supported.")))))

(defun read-jpeg-file (pathname)
  (with-open-file (stream pathname :direction :input :element-type '(unsigned-byte 8))
    (read-jpeg-stream stream)))

(defun write-jpeg-stream (stream image)
  (typecase image
    (8-bit-rgb-image
     (destructuring-bind (height width channels)
         (array-dimensions image)
       (declare (ignore channels))
       (let ((jpeg-array (make-array (* height width +ncomp-rgb+))))
         (loop for i below height
            do 
            (loop for j below width
               do 
               (let ((pixoff (* +ncomp-rgb+ (+ (* i width) j))))
                 (multiple-value-bind
                       (r g b)
                     (8-bit-rgb-pixel image i j)
                   (setf (aref jpeg-array pixoff) b
                         (aref jpeg-array (incf pixoff)) g
                         (aref jpeg-array (incf pixoff)) r)))))
         (jpeg::encode-image-stream stream jpeg-array +ncomp-rgb+ height width
                                    :sampling *rgb-sampling*))))

    (8-bit-rgba-image
     (destructuring-bind (height width channels)
         (array-dimensions image)
       (declare (ignore channels))
       (let ((jpeg-array (make-array (* height width +ncomp-rgb+))))
         (loop for i below height
            do 
              (loop for j below width
                 do 
                   (let ((pixoff (* +ncomp-rgb+ (+ (* i width) j))))
                     (multiple-value-bind
                           (r g b a)
                         ;; FIXME! For now we're just ignoring the alpha channel here!!
                         (delare (ignore a))
                         (8-bit-rgba-pixel image i j)
                       (setf (aref jpeg-array pixoff) b
                             (aref jpeg-array (incf pixoff)) g
                             (aref jpeg-array (incf pixoff)) r)))))
         (jpeg::encode-image-stream stream jpeg-array +ncomp-rgb+ height width
                                    :sampling *rgb-sampling*))))

    (t (error "Cannot write a JPEG image from ~A" (type-of image)))))

(defun write-jpeg-file (pathname image)
  (with-open-file (stream pathname
                          :direction :output
                          :element-type '(unsigned-byte 8)
                          :if-exists :supersede)
    (write-jpeg-stream stream image)
    pathname))



