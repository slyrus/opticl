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
                (read-from-string (format nil "make-~A" type))))
           `(progn
              (deftype ,type () '(image 1 ,bits))

              (defun ,ctor-function (height width)
                (make-array (list height width) 
                            :element-type '(unsigned-byte ,bits))))))))

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
                (read-from-string (format nil "make-~A" type))))
           `(progn
              (deftype ,type () '(image 3 ,bits))

              (defun ,ctor-function (height width)
                (make-array (list height width 3) 
                            :element-type '(unsigned-byte ,bits))))))))
  (frob-rgb-image 4)
  (frob-rgb-image 8)
  (frob-rgb-image 16))

(macrolet
    ((frob-rgba-image (bits)
       (let ((type
              (read-from-string (format nil "~A-bit-rgba-image" bits))))
         (let ((ctor-function
                (read-from-string (format nil "make-~A" type))))
           `(progn
              (deftype ,type () '(image 4 ,bits))

              (defun ,ctor-function (height width)
                (make-array (list height width 4) 
                            :element-type '(unsigned-byte ,bits))))))))
  (frob-rgba-image 4)
  (frob-rgba-image 8)
  (frob-rgba-image 16))


(defun get-image-dimensions (image-var env)
  #+(or sbcl ccl)
  (multiple-value-bind (binding-type localp declarations)
      (opticl-cltl2:variable-information image-var env)
    (declare (ignore binding-type localp))
    (let ((type-decl (find 'type declarations :key #'car)))
      (and type-decl
           (listp type-decl)
           (= (length type-decl) 4)
           (fourth type-decl)))))

(defconstant +max-image-channels+ 4)

(define-setf-expander pixel (image-var y x &environment env)
  (let ((image-dimensions (get-image-dimensions image-var env)))
    (if image-dimensions
        (let ((arity (or (and (= (length image-dimensions) 3)
                              (third image-dimensions))
                         1))
              (temp-y (gensym))
              (temp-x (gensym)))
          (if (= arity 1)
              (let ((store (gensym)))
                (values `(,temp-y ,temp-x)
                        `(,y ,x)
                        `(,store)
                        `(setf (aref ,image-var ,temp-y ,temp-x) ,store)
                        `(aref ,image-var ,temp-y ,temp-x)))
              (let ((stores (map-into (make-list arity) #'gensym)))
                (values `(,temp-y ,temp-x)
                        `(,y ,x)
                        stores
                        `(progn (setf ,@(loop for i from 0
                                           for store in stores
                                           collect `(aref ,image-var ,temp-y ,temp-x ,i)
                                           collect store))
                                (values ,@stores))
                        `(values ,@(loop for i from 0 below (length stores)
                                      collect `(aref ,image-var ,temp-y ,temp-x ,i)))))))
        (let ((syms (map-into (make-list +max-image-channels+) #'gensym)))
          (let ((temp-y (gensym))
                (temp-x (gensym)))
            (values `(,temp-y ,temp-x)
                    `(,y ,x)
                    syms
                    `(case (array-rank ,image-var)
                       (3 (let ((d (array-dimension ,image-var 2)))
                            (case d
                              (1
                               (values
                                (setf (aref ,image-var ,temp-y ,temp-x 0) ,(elt syms 0))))
                              (2
                               (values
                                (setf (aref ,image-var ,temp-y ,temp-x 0) ,(elt syms 0))
                                (setf (aref ,image-var ,temp-y ,temp-x 1) ,(elt syms 1))))
                              (3
                               (values
                                (setf (aref ,image-var ,temp-y ,temp-x 0) ,(elt syms 0))
                                (setf (aref ,image-var ,temp-y ,temp-x 1) ,(elt syms 1))
                                (setf (aref ,image-var ,temp-y ,temp-x 2) ,(elt syms 2))))
                              (4
                               (values
                                (setf (aref ,image-var ,temp-y ,temp-x 0) ,(elt syms 0))
                                (setf (aref ,image-var ,temp-y ,temp-x 1) ,(elt syms 1))
                                (setf (aref ,image-var ,temp-y ,temp-x 2) ,(elt syms 2))
                                (setf (aref ,image-var ,temp-y ,temp-x 3) ,(elt syms 3))))
                              (t (loop for i below d
                                    collect (setf (aref ,image-var ,temp-y ,temp-x i) (elt (list ,@syms) i)))))))
                       (2 (setf (aref ,image-var ,temp-y ,temp-x) ,(elt syms 0))))
                    `(case (array-rank ,image-var)
                       (3
                        (let ((d (array-dimension ,image-var 2)))
                          (case d
                            (1
                             (values
                              (aref ,image-var ,temp-y ,temp-x 0)))
                            (2
                             (values
                              (aref ,image-var ,temp-y ,temp-x 0)
                              (aref ,image-var ,temp-y ,temp-x 1)))
                            (3
                             (values
                              (aref ,image-var ,temp-y ,temp-x 0)
                              (aref ,image-var ,temp-y ,temp-x 1)
                              (aref ,image-var ,temp-y ,temp-x 2)))
                            (4
                             (values
                              (aref ,image-var ,temp-y ,temp-x 0)
                              (aref ,image-var ,temp-y ,temp-x 1)
                              (aref ,image-var ,temp-y ,temp-x 2)
                              (aref ,image-var ,temp-y ,temp-x 3)))
                            (t (values-list
                                (loop for i below d
                                   collect (aref ,image-var ,temp-y ,temp-x i)))))))
                       (2 (aref ,image-var ,temp-y ,temp-x)))))))))



(defmacro pixel (image-var y x &environment env)
  (let ((image-dimensions (get-image-dimensions image-var env)))
    (if image-dimensions
        (progn
          (case (length image-dimensions)
            (2 `(aref ,image-var ,y ,x))
            (3 `(values ,@(loop for i below (third image-dimensions)
                             collect `(aref ,image-var ,y ,x ,i))))))
        `(case (array-rank ,image-var)
           (2 (aref ,image-var ,y ,x))
           (3 (case (array-dimension ,image-var 2)
                (2 (values
                    (aref ,image-var ,y ,x 0)
                    (aref ,image-var ,y ,x 1)))
                (3 (values
                    (aref ,image-var ,y ,x 0)
                    (aref ,image-var ,y ,x 1)
                    (aref ,image-var ,y ,x 2)))
                (4 (values
                    (aref ,image-var ,y ,x 0)
                    (aref ,image-var ,y ,x 1)
                    (aref ,image-var ,y ,x 2)
                    (aref ,image-var ,y ,x 3)))))))))


(defun constrain (val min max)
  (let ((val (if (< val min) min val)))
    (if (> val max)
        max
        val)))

(defmacro with-image-bounds ((ymax-var xmax-var &optional (channels (gensym))) img &body body)
  `(let ((,ymax-var (array-dimension ,img 0))
         (,xmax-var (array-dimension ,img 1))
         (,channels (when (= (array-rank ,img) 3)
                      (array-dimension ,img 2))))
     (declare (ignorable ,channels))
     ,@body))


(defun pixel-in-bounds (img y x)
  (with-image-bounds (ymax xmax)
      img
    (and (>= y 0)
         (< y ymax)
         (>= x 0)
         (< x xmax))))
