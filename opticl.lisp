;;; Copyright (c) 2011 Cyrus Harmon, All rights reserved.
;;; See COPYRIGHT file for details.

(in-package :opticl)

(deftype image (&optional channels bits-per-channel)
  `(simple-array ,(if (numberp bits-per-channel)
                      `(unsigned-byte ,bits-per-channel)
                      bits-per-channel)
                 ,(if (numberp channels)
                      (if (= channels 1)
                          `(* *)
                          `(* * ,channels))
                      channels)))
(macrolet
    ((frob-gray-image (bits)
       (let ((type
              (intern (string-upcase (format nil "~A-bit-gray-image" bits))))
             (ctor-function
              (intern (string-upcase (format nil "make-~A-bit-gray-image" bits))))
             (pixel-function
              (intern (string-upcase (format nil "~A-bit-gray-pixel" bits)))))
         `(progn
            (deftype ,type () '(image 1 ,bits))

            (defun ,ctor-function (height width)
              (make-array (list height width) 
                          :element-type '(unsigned-byte ,bits)))

            (defun ,pixel-function (img y x)
              (declare (type fixnum y x))
              (declare (type ,type img))
              (declare (optimize (speed 3) (safety 0)))
              (aref img y x))
            (declaim (inline ,pixel-function))

            (defun (setf ,pixel-function) (val img y x)
              (declare (type fixnum y x))
              (declare (type ,type img))
              (declare (optimize (speed 3) (safety 0)))
              (setf (aref img y x) val))
            (declaim (inline (setf ,pixel-function)))))))
  (frob-gray-image 1)
  (frob-gray-image 2)
  (frob-gray-image 4)
  (frob-gray-image 8)
  (frob-gray-image 16))

(macrolet
    ((frob-rgb-image (bits)
       (let ((type
              (intern (string-upcase (format nil "~A-bit-rgb-image" bits))))
             (ctor-function
              (intern (string-upcase (format nil "make-~A-bit-rgb-image" bits))))
             (pixel-function
              (intern (string-upcase (format nil "~A-bit-rgb-pixel" bits)))))
         `(progn
            (deftype ,type () '(image 3 ,bits))

            (defun ,ctor-function (height width)
              (make-array (list height width 3) 
                          :element-type '(unsigned-byte ,bits)))

            (defun ,pixel-function (img y x)
              (declare (type fixnum y x))
              (declare (type ,type img))
              (declare (optimize (speed 3) (safety 0)))
              (values (aref img y x 0)
                      (aref img y x 1)
                      (aref img y x 2)))
            (declaim (inline ,pixel-function))
            
            (defsetf ,pixel-function (img y x) (r g b)
              `(locally
                   (declare (type ,',type ,img)
                            (optimize (speed 3) (safety 0)))
                 (setf (values (aref ,img ,y ,x 0)
                               (aref ,img ,y ,x 1)
                               (aref ,img ,y ,x 2))
                       (values ,r ,g ,b))))))))
  (frob-rgb-image 4)
  (frob-rgb-image 5)
  (frob-rgb-image 8)
  (frob-rgb-image 16))

(macrolet
    ((frob-rgba-image (bits)
       (let ((type
              (intern (string-upcase (format nil "~A-bit-rgba-image" bits))))
             (ctor-function
              (intern (string-upcase (format nil "make-~A-bit-rgba-image" bits))))
             (pixel-function
              (intern (string-upcase (format nil "~A-bit-rgba-pixel" bits)))))
         `(progn
            (deftype ,type () '(image 4 ,bits))

            (defun ,ctor-function (height width)
              (make-array (list height width 4) 
                          :element-type '(unsigned-byte ,bits)))

            (defun ,pixel-function (img y x)
              (declare (type fixnum y x))
              (declare (type ,type img))
              (declare (optimize (speed 3) (safety 0)))
              (values (aref img y x 0)
                      (aref img y x 1)
                      (aref img y x 2)
                      (aref img y x 3)))
            (declaim (inline ,pixel-function))
            
            (defsetf ,pixel-function (img y x) (r g b a)
              `(locally
                   (declare (type ,',type ,img)
                            (optimize (speed 3) (safety 0)))
                 (setf (values (aref ,img ,y ,x 0)
                               (aref ,img ,y ,x 1)
                               (aref ,img ,y ,x 2)
                               (aref ,img ,y ,x 3))
                       (values ,r ,g ,b ,a))))))))
  (frob-rgba-image 4)
  (frob-rgba-image 5)
  (frob-rgba-image 8)
  (frob-rgba-image 16))

