
(in-package :opticl)

(deftype image (&optional channels bits-per-channel)
  `(simple-array (unsigned-byte ,bits-per-channel)
                 ,(if (= channels 1)
                      `(* *)
                      `(* * ,channels))))

(deftype 1-bit-gray-image () '(image 1 1))
(defun make-1-bit-gray-image (height width)
  (make-array (list height width) 
              :element-type '(unsigned-byte 1)))

(defun 1-bit-gray-pixel (img y x)
  (declare (type fixnum y x))
  (declare (type 1-bit-gray-image img))
  (declare (optimize (speed 3) (safety 0)))
  (aref img y x))

(defun (setf 1-bit-gray-pixel) (val img y x)
  (declare (type fixnum y x))
  (declare (type 1-bit-gray-image img))
  (declare (optimize (speed 3) (safety 0)))
  (setf (aref img y x) val))

(deftype 4-bit-gray-image () '(image 1 4))
(defun make-4-bit-gray-image (height width)
  (make-array (list height width) 
              :element-type '(unsigned-byte 4)))

(deftype 8-bit-gray-image () '(image 1 8))
(defun make-8-bit-gray-image (height width)
  (make-array (list height width) 
              :element-type '(unsigned-byte 8)))

(deftype 16-bit-gray-image () '(image 1 16))
(defun make-16-bit-gray-image (height width)
  (make-array (list height width) 
              :element-type '(unsigned-byte 16)))

(deftype 4-bit-rgb-image () '(image 3 4))
(defun make-4-bit-rgb-image (height width)
  (make-array (list height width 3) 
              :element-type '(unsigned-byte 4)))

(deftype 8-bit-rgb-image () '(image 3 8))
(defun make-8-bit-rgb-image (height width)
  (make-array (list height width 3) 
              :element-type '(unsigned-byte 8)))

(deftype 16-bit-rgb-image () '(image 3 16))
(defun make-16-bit-rgb-image (height width)
  (make-array (list height width 3) 
              :element-type '(unsigned-byte 16)))


(deftype 4-bit-argb-image () '(image 4 4))
(defun make-4-bit-argb-image (height width)
  (make-array (list height width 4) 
              :element-type '(unsigned-byte 4)))

(deftype 8-bit-argb-image () '(image 4 8))
(defun make-8-bit-argb-image (height width)
  (make-array (list height width 4) 
              :element-type '(unsigned-byte 8)))

(deftype 16-bit-argb-image () '(image 4 16))
(defun make-16-bit-argb-image (height width)
  (make-array (list height width 4) 
              :element-type '(unsigned-byte 16)))


