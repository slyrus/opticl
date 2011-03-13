
;;; Copyright (c) 2011 Cyrus Harmon, All rights reserved.
;;; See COPYRIGHT file for details.

(in-package :opticl)

;;; Note: a GIF can contain multiple images. We're going to ignore
;;; this little detail and just return one image for the moment.
(defun read-gif-stream (stream)
  (let ((data-stream (skippy:read-data-stream stream)))
    (let ((image (elt (skippy:images data-stream) 0))
          (color-table (skippy:color-table data-stream)))
      (with-accessors ((height skippy:height)
                       (width skippy:width)
                       (image-data skippy:image-data))
          image
        (let ((newimg (make-8-bit-rgb-image height width)))
          (set-pixels (i j)
              newimg
            (skippy:color-rgb
             (skippy:color-table-entry color-table (skippy:pixel-ref image j i))))
          newimg)))))

(defun read-gif-file (pathname)
  (with-open-file (stream pathname :direction :input :element-type '(unsigned-byte 8))
    (read-gif-stream stream)))

(defun write-gif-stream (stream image)
  (declare (ignore stream image))
  (error "writing gifs not yet supported!"))

(defun write-gif-file (pathname image)
  (with-open-file (stream pathname :direction :output
                          :if-exists :supersede
                          :element-type '(unsigned-byte 8))
    (write-gif-stream stream image)
    (truename pathname)))
