
(in-package :opticl)

(defparameter *image-file-reader-hash-table* (make-hash-table))
(map nil (lambda (z)
           (destructuring-bind (x y) z
             (setf (gethash x *image-file-reader-hash-table*) y)))
     '((:tiff read-tiff-file)
      (:tif read-tiff-file)
      (:jpeg read-jpeg-file)
      (:jpg read-jpeg-file)
      (:png read-png-file)
      (:pbm read-pbm-file)
      (:pgm read-pgm-file)
      (:ppm read-ppm-file)))

(defparameter *image-file-writer-hash-table* (make-hash-table))
(map nil (lambda (z)
           (destructuring-bind (x y) z
             (setf (gethash x *image-file-writer-hash-table*) y)))
     '((:tiff write-tiff-file)
       (:tif write-tiff-file)
       (:jpeg write-jpeg-file)
       (:jpg write-jpeg-file)
       (:png write-png-file)
       (:pbm write-pbm-file)
       (:pgm write-pgm-file)
       (:ppm write-ppm-file)))

(defun get-image-file-reader (file)
  (typecase file
    (string (get-image-file-reader (pathname file)))
    (pathname
     (let* ((type (pathname-type file))
            (key (intern (string-upcase type) :keyword)))
       (gethash key *image-file-reader-hash-table*)))))

(defun get-image-file-writer (file)
  (typecase file
    (string (get-image-file-writer (pathname file)))
    (pathname
     (let* ((type (pathname-type file))
            (key (intern (string-upcase type) :keyword)))
       (gethash key *image-file-writer-hash-table*)))))

(defun read-image-file (file)
  (let ((fn (get-image-file-reader file)))
    (if fn
        (funcall fn file)
        (error "Cannot read image file: ~S" file))))

(defun write-image-file (file image)
  (let ((fn (get-image-file-writer file)))
    (if fn
        (funcall fn file image)
        (error "Cannot write image file: ~S" file))))
