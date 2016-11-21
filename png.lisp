;;; Copyright (c) 2011 Cyrus Harmon, All rights reserved.
;;; See COPYRIGHT file for details.

(in-package :opticl)

(defun read-png-stream (stream)
  (let* ((png (png-read:read-png-datastream stream))
	 (colour-type (png-read:colour-type png))
	 (bit-depth (png-read:bit-depth png))
	 (width (png-read:width png))
	 (height (png-read:height png))
	 (image-data (png-read:image-data png))
	 (transparency (png-read::transparency png)))
    (flet ((get-pixel-grey (i j)
	     (aref image-data j i))
	   (get-pixel-grey-alpha (i j)
	     (let ((brightness (aref image-data j i 0)))
	       (values brightness
		       brightness
		       brightness
		       (aref image-data j i 1))))
	   (get-pixel-grey-tmap (i j)
	     (let ((brightness (aref image-data j i)))
	       (values brightness
		       brightness
		       brightness
		       (aref transparency j i))))
	   (get-pixel-rgba (i j)
	     (values (aref image-data j i 0)
		     (aref image-data j i 1)
		     (aref image-data j i 2)
		     (aref image-data j i 3)))
	   (get-pixel-rgb-tmap (i j)
	     (values (aref image-data j i 0)
		     (aref image-data j i 1)
		     (aref image-data j i 2)
		     (aref transparency j i)))
	   (get-pixel-rgb (i j)
	     (values (aref image-data j i 0)
		     (aref image-data j i 1)
		     (aref image-data j i 2))))
      (multiple-value-bind (constructor get-pixel-fn)
	  (case bit-depth
	    (8 (case transparency
		 ((nil)
		  (case colour-type
		    ((:truecolor :indexed-colour)
		     (values #'make-8-bit-rgb-image
			     #'get-pixel-rgb))
		    (:truecolor-alpha
		     (values #'make-8-bit-rgba-image
			     #'get-pixel-rgba))
		    (:greyscale-alpha
		     (values #'make-8-bit-rgba-image
			     #'get-pixel-grey-alpha))
		    (:greyscale
		     (values #'make-8-bit-gray-image
			     #'get-pixel-grey))))
		 (t
		  (values #'make-8-bit-rgba-image
			  (case colour-type
			    ((:truecolor :indexed-colour)
			     #'get-pixel-rgb-tmap)
			    (:greyscale
			     #'get-pixel-grey-tmap)))))))
	(unless get-pixel-fn
	  (error "unable to read PNG image -- fix read-png-stream!"))
	(let ((img (funcall constructor height width)))
	  (dotimes (i height img)
	    (dotimes (j width)
	      (setf (pixel img i j)
		    (funcall get-pixel-fn i j)))))))))

(defun read-png-file (pathname)
  (with-open-file (stream pathname :direction :input :element-type '(unsigned-byte 8))
    (read-png-stream stream)))


(defun write-png-stream (stream image)
  (typecase image
    (8-bit-rgb-image
     (with-image-bounds (ymax xmax)
         image
       (let ((displaced
              (make-array (* ymax xmax 3)
                          :element-type '(unsigned-byte 8)
                          :initial-contents
                          (make-array (* ymax xmax 3)
                                      :element-type '(unsigned-byte 8)
                                      :displaced-to image))))
         (zpng:write-png-stream 
          (make-instance 'zpng:png
                         :color-type :truecolor
                         :height ymax
                         :width xmax
                         :bpp 8
                         :image-data displaced)
          stream))))
    (8-bit-rgba-image
     (with-image-bounds (ymax xmax)
         image
       (let ((displaced
              (make-array (* ymax xmax 4)
                          :element-type '(unsigned-byte 8)
                          :initial-contents
                          (make-array (* ymax xmax 4)
                                      :element-type '(unsigned-byte 8)
                                      :displaced-to image))))
         (zpng:write-png-stream 
          (make-instance 'zpng:png
                         :color-type :truecolor-alpha
                         :height ymax
                         :width xmax
                         :bpp 8
                         :image-data displaced)
          stream))))
    (8-bit-gray-image
     (with-image-bounds (ymax xmax)
         image
       (let ((displaced
              (make-array (* ymax xmax)
                          :element-type '(unsigned-byte 8)
                          :initial-contents
                          (make-array (* ymax xmax)
                                      :element-type '(unsigned-byte 8)
                                      :displaced-to image))))
         (zpng:write-png-stream 
          (make-instance 'zpng:png
                         :color-type :grayscale
                         :height ymax
                         :width xmax
                         :bpp 8
                         :image-data displaced)
          stream))))
    (t (error "No PNG writing support for this image type."))))

(defun write-png-file (pathname image)
  (with-open-file (stream pathname :direction :output
                          :if-exists :supersede
                          :element-type '(unsigned-byte 8))
    (write-png-stream stream image)
    (truename pathname)))
