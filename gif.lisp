
;;; Copyright (c) 2011 Cyrus Harmon, All rights reserved.
;;; See COPYRIGHT file for details.

(in-package :opticl)

;;; Note: a GIF can contain multiple images. We're going to ignore
;;; this little detail and just return one image for the moment.

(defun skippy-image-to-8-bit-rgb-image (skippy-image color-table)
  (with-accessors ((height skippy:height)
                   (width skippy:width)
                   (image-data skippy:image-data))
      skippy-image
    (let ((new-image (make-8-bit-rgb-image height width)))
      (set-pixels (i j) new-image
        (skippy:color-rgb
         (skippy:color-table-entry color-table
                                   (skippy:pixel-ref skippy-image j i))))
      new-image)))

(defun read-gif-stream (stream)
  (let ((data-stream (skippy:read-data-stream stream)))
    (let ((color-table (skippy:color-table data-stream)))
      (values-list 
       (loop for image across (skippy:images data-stream)
          collect (skippy-image-to-8-bit-rgb-image image color-table))))))

(defun read-gif-file (pathname)
  (with-open-file (stream pathname :direction :input :element-type '(unsigned-byte 8))
    (read-gif-stream stream)))

(defun 8-bit-rgb-image-to-skippy-image (image color-table)
  (with-image-bounds (height width)
      image
    (let ((gif-image (skippy:make-image :height height :width width)))
      (do-pixels (i j) image
        (multiple-value-bind (r g b)
            (pixel image i j)
          (let ((color (skippy:ensure-color (skippy:rgb-color r g b)
                                            color-table)))
            (setf (skippy:pixel-ref gif-image j i) color))))
      gif-image)))

(defun write-gif-stream (stream image)
  (with-image-bounds (height width)
      image
    (let* ((data-stream (skippy:make-data-stream
                         :height height :width width :color-table t))
           (gif-image (8-bit-rgb-image-to-skippy-image image
                                                       (skippy:color-table data-stream))))
      (skippy:add-image gif-image data-stream)
      (skippy:write-data-stream data-stream stream)))) 

(defun write-gif-file (pathname image)
  (with-open-file (stream pathname :direction :output
                          :if-exists :supersede
                          :element-type '(unsigned-byte 8))
    (write-gif-stream stream image)
    (truename pathname)))
