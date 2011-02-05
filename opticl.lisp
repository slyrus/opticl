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

(defmacro check-bounds ((img y x) &body body)
  (let ((ymax (gensym)) (xmax (gensym)))
    `(let ((,ymax (1- (array-dimension ,img 0)))
           (,xmax (1- (array-dimension ,img 1))))
       (if (and (<= 0 ,y ,ymax)
                (<= 0 ,x ,xmax))
           ,@body))))

(macrolet
    ((frob-gray-image (bits)
       (let ((type
              (read-from-string (format nil "~A-bit-gray-image" bits))))
         (let ((ctor-function
                (read-from-string (format nil "make-~A" type)))
               (safe-pixel-function
                (read-from-string (format nil "pixel/~A" type)))
               (unsafe-pixel-function
                (read-from-string (format nil "pixel*/~A" type)))
               (safe-set-pixel-function
                (read-from-string (format nil "set-pixel/~A" type)))
               (unsafe-set-pixel-function
                (read-from-string (format nil "set-pixel*/~A" type))))
           `(progn
              (deftype ,type () '(image 1 ,bits))

              (defun ,ctor-function (height width)
                (make-array (list height width) 
                            :element-type '(unsigned-byte ,bits)))

              (defun ,safe-pixel-function (img y x)
                (declare (type fixnum y x))
                (declare (type ,type img))
                (check-bounds (img y x)
                  (aref img y x)
                  0))
              (declaim (inline ,safe-pixel-function))
              
              (defsetf ,safe-pixel-function (img y x) (k)
                `(check-bounds (,img ,y ,x)
                   (setf (aref ,img ,y ,x) ,k)))
              
              (defun ,unsafe-pixel-function (img y x)
                (declare (type fixnum y x))
                (declare (type ,type img))
                (declare (optimize (speed 3) (safety 0)))
                (aref img y x))
              (declaim (inline ,unsafe-pixel-function))

              (defun (setf ,unsafe-pixel-function) (val img y x)
                (declare (type fixnum y x))
                (declare (type ,type img))
                (declare (optimize (speed 3) (safety 0)))
                (setf (aref img y x) val))
              (declaim (inline (setf ,unsafe-pixel-function)))

              (defun ,safe-set-pixel-function (img y x k)
                (declare (type fixnum y x))
                (declare (type ,type img))
                (check-bounds (img y x)
                  (setf (aref img y x) k)))

              (defun ,unsafe-set-pixel-function (img y x k)
                (declare (type fixnum y x))
                (declare (type ,type img))
                (declare (optimize (speed 3) (safety 0)))
                (check-bounds (img y x)
                  (setf (aref img y x) k))))))))

  (frob-gray-image 1)
  (frob-gray-image 2)
  (frob-gray-image 4)
  (frob-gray-image 8)
  (frob-gray-image 16))

(macrolet
    ((frob-rgb-image (bits)
       (let ((type
              (read-from-string (format nil "~A-bit-rgb-image" bits))))
         (let ((ctor-function
                (read-from-string (format nil "make-~A" type)))
               (safe-pixel-function
                (read-from-string (format nil "pixel/~A" type)))
               (unsafe-pixel-function
                (read-from-string (format nil "pixel*/~A" type)))
               (safe-set-pixel-function
                (read-from-string (format nil "set-pixel/~A" type)))
               (unsafe-set-pixel-function
                (read-from-string (format nil "set-pixel*/~A" type))))
           `(progn
              (deftype ,type () '(image 3 ,bits))

              (defun ,ctor-function (height width)
                (make-array (list height width 3) 
                            :element-type '(unsigned-byte ,bits)))

              (defun ,safe-pixel-function (img y x)
                (declare (type fixnum y x))
                (declare (type ,type img))
                (check-bounds (img y x)
                  (values (aref img y x 0)
                          (aref img y x 1)
                          (aref img y x 2))
                  (values 0 0 0)))
              (declaim (inline ,safe-pixel-function))
            
              (defun ,safe-set-pixel-function (img y x r g b)
                (declare (type fixnum y x))
                (declare (type ,type img))
                (check-bounds (img y x)
                  (setf (values (aref img y x 0)
                                (aref img y x 1)
                                (aref img y x 2))
                        (values r g b))))
              
              (defsetf ,safe-pixel-function (img y x) (r g b)
                `(check-bounds (,img ,y ,x)
                   (setf (values (aref ,img ,y ,x 0)
                                 (aref ,img ,y ,x 1)
                                 (aref ,img ,y ,x 2))
                         (values ,r ,g ,b))))
            
              (defun ,unsafe-pixel-function (img y x)
                (declare (type fixnum y x))
                (declare (type ,type img))
                (declare (optimize (speed 3) (safety 0)))
                (values (aref img y x 0)
                        (aref img y x 1)
                        (aref img y x 2)))
              (declaim (inline ,unsafe-pixel-function))
            
              (defsetf ,unsafe-pixel-function (img y x) (r g b)
                `(locally
                     (declare (type ,',type ,img)
                              (optimize (speed 3) (safety 0)))
                   (setf (values (aref ,img ,y ,x 0)
                                 (aref ,img ,y ,x 1)
                                 (aref ,img ,y ,x 2))
                         (values ,r ,g ,b)))))))))
  (frob-rgb-image 4)
  (frob-rgb-image 8)
  (frob-rgb-image 16))

(macrolet
    ((frob-rgba-image (bits)
       (let ((type
              (read-from-string (format nil "~A-bit-rgba-image" bits))))
         (let ((ctor-function
                (read-from-string (format nil "make-~A" type)))
               (safe-pixel-function
                (read-from-string (format nil "pixel/~A" type)))
               (safe-set-pixel-function
                (read-from-string (format nil "set-pixel/~A" type)))
               (unsafe-pixel-function
                (read-from-string (format nil "pixel*/~A" type)))
               (unsafe-set-pixel-function
                (read-from-string (format nil "set-pixel*/~A" type))))
           `(progn
              (deftype ,type () '(image 4 ,bits))

              (defun ,ctor-function (height width)
                (make-array (list height width 4) 
                            :element-type '(unsigned-byte ,bits)))

              (defun ,safe-pixel-function (img y x)
                (declare (type fixnum y x))
                (declare (type ,type img))
                (check-bounds (img y x)
                  (values (aref img y x 0)
                          (aref img y x 1)
                          (aref img y x 2)
                          (aref img y x 3))
                  (values 0 0 0)))
              (declaim (inline ,safe-pixel-function))
            
              (defun ,safe-set-pixel-function (img y x r g b a)
                (declare (type fixnum y x))
                (declare (type ,type img))
                (check-bounds (img y x)
                  (setf (values (aref img y x 0)
                                (aref img y x 1)
                                (aref img y x 2)
                                (aref img y x 3))
                        (values r g b a))))

              (defsetf ,safe-pixel-function (img y x) (r g b a)
                `(check-bounds (,img ,y ,x)
                   (setf (values (aref ,img ,y ,x 0)
                                 (aref ,img ,y ,x 1)
                                 (aref ,img ,y ,x 2)
                                 (aref ,img ,y ,x 3))
                         (values ,r ,g ,b ,a))))
              
              (defun ,unsafe-pixel-function (img y x)
                (declare (type fixnum y x))
                (declare (type ,type img))
                (declare (optimize (speed 3) (safety 0)))
                (values (aref img y x 0)
                        (aref img y x 1)
                        (aref img y x 2)
                        (aref img y x 3)))
              (declaim (inline ,unsafe-pixel-function))
            
              (defsetf ,unsafe-pixel-function (img y x) (r g b a)
                `(locally
                     (declare (type ,',type ,img)
                              (optimize (speed 3) (safety 0)))
                   (setf (values (aref ,img ,y ,x 0)
                                 (aref ,img ,y ,x 1)
                                 (aref ,img ,y ,x 2)
                                 (aref ,img ,y ,x 3))
                         (values ,r ,g ,b ,a)))))))))
  (frob-rgba-image 4)
  (frob-rgba-image 8)
  (frob-rgba-image 16))


(defun pixel (img &rest vals)
  (etypecase img
    (1-bit-gray-image (apply #'pixel/1-bit-gray-image img vals))
    (2-bit-gray-image (apply #'pixel/2-bit-gray-image img vals))
    (4-bit-gray-image (apply #'pixel/4-bit-gray-image img vals))
    (8-bit-gray-image (apply #'pixel/8-bit-gray-image img vals))
    (16-bit-gray-image (apply #'pixel/16-bit-gray-image img vals))
    
    (4-bit-rgb-image (apply #'pixel/4-bit-rgb-image img vals))
    (8-bit-rgb-image (apply #'pixel/8-bit-rgb-image img vals))
    (16-bit-rgb-image (apply #'pixel/16-bit-rgb-image img vals))

    (4-bit-rgba-image (apply #'pixel/4-bit-rgba-image img vals))
    (8-bit-rgba-image (apply #'pixel/8-bit-rgba-image img vals))
    (16-bit-rgba-image (apply #'pixel/16-bit-rgba-image img vals))))

(defconstant +max-image-channels+ 64)

(define-setf-expander pixel (img y x &environment env)
  (multiple-value-bind (temps subforms store-vars setter getter)
      (get-setf-expansion img env)
    (declare (ignore store-vars setter))
    (let ((syms (map-into (make-list +max-image-channels+) #'gensym)))
      (values temps
              subforms
              syms
              `(check-bounds (,img ,y ,x)
                 (case (array-rank ,getter)
                   (3 (let ((d (array-dimension ,getter 2)))
                        (case d
                          (1
                           (values
                            (setf (aref ,getter ,y ,x 0) ,(elt syms 0))))
                          (2
                           (values
                            (setf (aref ,getter ,y ,x 0) ,(elt syms 0))
                            (setf (aref ,getter ,y ,x 1) ,(elt syms 1))))
                          (3
                           (values
                            (setf (aref ,getter ,y ,x 0) ,(elt syms 0))
                            (setf (aref ,getter ,y ,x 1) ,(elt syms 1))
                            (setf (aref ,getter ,y ,x 2) ,(elt syms 2))))
                          (4
                           (values
                            (setf (aref ,getter ,y ,x 0) ,(elt syms 0))
                            (setf (aref ,getter ,y ,x 1) ,(elt syms 1))
                            (setf (aref ,getter ,y ,x 2) ,(elt syms 2))
                            (setf (aref ,getter ,y ,x 3) ,(elt syms 3))))
                          (t (loop for i below d
                                collect (setf (aref ,getter ,y ,x i) (elt (list ,@syms) i)))))))
                   (2 (setf (aref ,getter ,y ,x) ,(elt syms 0))))
                 (values))
              `(check-bounds (,img ,y ,x)
                 (case (array-rank ,getter)
                   (3
                    (let ((d (array-dimension ,getter 2)))
                      (case d
                        (1
                         (values
                          (aref ,getter ,y ,x 0)))
                        (2
                         (values
                          (aref ,getter ,y ,x 0)
                          (aref ,getter ,y ,x 1)))
                        (3
                         (values
                          (aref ,getter ,y ,x 0)
                          (aref ,getter ,y ,x 1)
                          (aref ,getter ,y ,x 2)))
                        (4
                         (values
                          (aref ,getter ,y ,x 0)
                          (aref ,getter ,y ,x 1)
                          (aref ,getter ,y ,x 2)
                          (aref ,getter ,y ,x 3)))
                        (t (values-list
                            (loop for i below d
                               collect (aref ,getter ,y ,x i)))))))
                   (2 (aref ,getter ,y ,x)))
                 (values))))))
