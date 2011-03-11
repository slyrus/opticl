;;; Copyright (c) 2011 Cyrus Harmon, All rights reserved.
;;; See COPYRIGHT file for details.

(in-package :opticl)

(deftype image (&key channels element-type)
  `(simple-array ,(if element-type
                      element-type
                      '*)
                 ,(if (numberp channels)
                      (if (= channels 1)
                          `(* *)
                          `(* * ,channels))
                      channels)))

(deftype gray-image (&key element-type)
  `(simple-array ,(if element-type
                      element-type
                      '*)
                 (* *)))

(deftype rgb-image (&key element-type)
  `(simple-array ,(if element-type
                      element-type
                      '*)
                 (* * 3)))

(deftype rgba-image (&key element-type)
  `(simple-array ,(if element-type
                      element-type
                      '*)
                 (* * 4)))

(deftype integer-image (&optional channels bits-per-channel)
  `(simple-array ,(if (numberp bits-per-channel)
                      `(unsigned-byte ,bits-per-channel)
                      bits-per-channel)
                 ,(if (numberp channels)
                      (if (= channels 1)
                          `(* *)
                          `(* * ,channels))
                      channels)))

(deftype single-float-image (&optional channels)
  `(simple-array single-float
                 ,(if (numberp channels)
                      (if (= channels 1)
                          `(* *)
                          `(* * ,channels))
                      channels)))

(deftype double-float-image (&optional channels)
  `(simple-array double-float
                 ,(if (numberp channels)
                      (if (= channels 1)
                          `(* *)
                          `(* * ,channels))
                      channels)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *image-types*
    '((1-bit-gray-image '(integer-image 1 1) '(unsigned-byte 1) 1)
      (2-bit-gray-image '(integer-image 1 2) '(unsigned-byte 2) 1)
      (4-bit-gray-image '(integer-image 1 4) '(unsigned-byte 4) 1)
      (8-bit-gray-image '(integer-image 1 8) '(unsigned-byte 8) 1)
      (16-bit-gray-image '(integer-image 1 16) '(unsigned-byte 16) 1)
      (32-bit-gray-image '(integer-image 1 32) '(unsigned-byte 32) 1)
      (single-float-gray-image '(single-float-image 1) 'single-float 1)
      (double-float-gray-image '(double-float-image 1) 'double-float 1)

      (4-bit-rgb-image '(integer-image 3 4) '(unsigned-byte 4) 3)
      (8-bit-rgb-image '(integer-image 3 8) '(unsigned-byte 8) 3)
      (16-bit-rgb-image '(integer-image 3 16) '(unsigned-byte 16) 3)
      (single-float-rgb-image '(single-float-image 3) 'single-float 3)
      (double-float-rgb-image '(double-float-image 3) 'double-float 3)

      (4-bit-rgba-image '(integer-image 4 4) '(unsigned-byte 4) 4)
      (8-bit-rgba-image '(integer-image 4 8) '(unsigned-byte 8) 4)
      (16-bit-rgba-image '(integer-image 4 16) '(unsigned-byte 16) 4)
      (single-float-rgba-image '(single-float-image 4) 'single-float 4)
      (double-float-rgba-image '(double-float-image 4) 'double-float 4)
      )))

(macrolet
    ((frob-image (name image-type element-type num-channels)
       (let ((type
              (read-from-string (format nil "~A" name))))
         (let ((ctor-function
                (read-from-string (format nil "make-~A" type))))
           `(progn
              (deftype ,type () ,image-type)
              (defun ,ctor-function (height width &key
                                     (initial-element nil initial-element-p)
                                     (initial-contents nil initial-contents-p))
                (apply #'make-array (append (list height width)
                                            (when (> ,num-channels 1)
                                              (list ,num-channels)))
                       :element-type ,element-type
                       (append
                        (when initial-element-p
                          `(:initial-element ,initial-element))
                        (when initial-contents-p
                          `(:initial-contents ,initial-contents)))))))))
     (frobber ()
       `(progn
          ,@(loop for (name image-type element-type num-channels) in *image-types*
               collect 
               `(frob-image ,name ,image-type ,element-type ,num-channels)))))
  (frobber))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %get-image-dimensions (image-var env)
    #+(or sbcl ccl)
    (when (symbolp image-var)
      (multiple-value-bind (binding-type localp declarations)
          (opticl-cltl2:variable-information image-var env)
        (declare (ignore binding-type localp))
        (let ((type-decl (find 'type declarations :key #'car)))
          (and type-decl
               (listp type-decl)
               (= (length type-decl) 4)
               (fourth type-decl)))))))

(defconstant +max-image-channels+ 4)

(define-setf-expander pixel (image-var y x &environment env)
  (let ((image-dimensions (%get-image-dimensions image-var env)))
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
                    `(ecase (array-rank ,image-var)
                       (3 (let ((d (array-dimension ,image-var 2)))
                            (ecase d
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
                    `(ecase (array-rank ,image-var)
                       (3
                        (let ((d (array-dimension ,image-var 2)))
                          (ecase d
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
  (let ((image-dimensions (%get-image-dimensions image-var env)))
    (if image-dimensions
        (progn
          (ecase (length image-dimensions)
            (2 `(aref ,image-var ,y ,x))
            (3 `(values ,@(loop for i below (third image-dimensions)
                             collect `(aref ,image-var ,y ,x ,i))))))
        `(ecase (array-rank ,image-var)
           (2 (aref ,image-var ,y ,x))
           (3 (ecase (array-dimension ,image-var 2)
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

(defun pixel* (image y x)
  "pixel* returns the value(s) at position y, x as a list. This
function conses, but occasionally one wants the image intensity values
as a list, rather than as multiple values. This is a convenience
function to provide this, largely to provide symmetry
with (setf (pixel* ...) ...)"
  (multiple-value-list (pixel image y x)))

(defmacro set-pixel* (image y x list)
  `(setf (pixel ,image ,y ,x) (values-list ,list)))

(defsetf pixel* set-pixel*
  "(setf (pixel* img y x) list) sets the values of pixel y, x in img
to the values contained in list. (setf (pixel ...) ...) is the more
efficient way to do this, but if one wants to pass a set of values as
a list instead of as multiple-values (for named colors perhaps), this
function does that.")

(defmacro with-image-bounds ((ymax-var xmax-var &optional (channels (gensym))) img &body body)
  `(let ((,ymax-var (array-dimension ,img 0))
         (,xmax-var (array-dimension ,img 1))
         (,channels (when (= (array-rank ,img) 3)
                      (array-dimension ,img 2))))
     (declare (ignorable ,channels))
     ,@body))

(defmacro do-pixels ((i-var j-var) image &body body)
  (alexandria:once-only (image)
    (alexandria:with-gensyms (height width)
      `(with-image-bounds (,height ,width) ,image
         (loop for ,i-var below ,height
            do (loop for ,j-var below ,width
                  do ,@body))))))

(defmacro set-pixels ((i-var j-var) image &body body)
  (alexandria:once-only (image)
    (alexandria:with-gensyms (height width)
      `(with-image-bounds (,height ,width) ,image
         (loop for ,i-var below ,height
            do (loop for ,j-var below ,width
                  do (setf (pixel ,image ,i-var ,j-var)
                           (progn
                             ,@body))))))))

(defmacro do-region-pixels ((i-var j-var y1 x1 y2 x2) image &body body)
  (declare (ignorable image))
  `(loop for ,i-var from ,y1 below ,y2
      do (loop for ,j-var from ,x1 below ,x2
            do ,@body)))

(defmacro set-region-pixels ((i-var j-var y1 x1 y2 x2) image &body body)
  (declare (ignorable image))
  `(loop for ,i-var from ,y1 below ,y2
      do (loop for ,j-var from ,x1 below ,x2
            do (setf (pixel ,image ,i-var ,j-var)
                     (progn
                       ,@body)))))

(defun clear-image (image)
  (with-image-bounds (height width channels)
      image
    (declare (ignore height width))
    (if channels
        (ecase channels
          (2 (do-pixels (i j) image
               (setf (pixel image i j) (values 0 0))))
          (3 (do-pixels (i j) image
               (setf (pixel image i j) (values 0 0 0))))
          (4 (do-pixels (i j) image
               (setf (pixel image i j) (values 0 0 0 0)))))
        (do-pixels (i j) image
               (setf (pixel image i j) 0))))
  image)

(defun mean (&rest numbers)
  (/ (apply #'+ numbers) (length numbers)))

(defun convert-image-to-grayscale (image)
  (etypecase image
    (gray-image image)
    ((or rgb-image rgba-image)
     (with-image-bounds (y x channels)
         image
       (let* ((type (array-element-type image))
              (gray-image (make-array (list y x) :element-type type)))
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
                       (coerce (mean r g b) type)))))
         gray-image)))))

(defun convert-image-to-grayscale-luminance (image)
  (etypecase image
    (gray-image image)
    ((or rgb-image rgba-image)
     (with-image-bounds (y x channels)
         image
       (let* ((type (array-element-type image))
              (gray-image (make-array (list y x) :element-type type)))
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
         gray-image)))))


(defun convert-image-to-rgb (image)
  (etypecase image
    (gray-image
     (with-image-bounds (y x channels)
         image
       (let* ((type (array-element-type image))
              (rgb-image (make-array (list y x 3) :element-type type)))
         (do-pixels (i j)
             image
           (let ((val (pixel image i j)))
             (setf (pixel rgb-image i j) 
                   (values val val val))))
         rgb-image)))
    (rgb-image image)
    (rgba-image
     (with-image-bounds (y x channels)
         image
       (let* ((type (array-element-type image))
              (rgb-image (make-array (list y x 3) :element-type type)))
         (do-pixels (i j)
             image
           (setf (pixel rgb-image i j) 
                 (pixel image i j)))
         rgb-image)))))

(defun convert-image-to-rgba (image)
  (declare (optimize (debug 3)))
  (etypecase image
    (gray-image
     (with-image-bounds (y x channels)
         image
       (let* ((type (array-element-type image))
              (rgba-image (make-array (list y x 4) :element-type type)))
         (do-pixels (i j)
             image
           (let ((val (pixel image i j)))
             (setf (pixel rgba-image i j) 
                   (values val val val 255))))
         rgba-image)))
    (rgb-image 
     (with-image-bounds (y x channels)
         image
       (let* ((type (array-element-type image))
              (rgba-image (make-array (list y x 4) :element-type type)))
         (do-pixels (i j)
             image
           (setf (pixel* rgba-image i j) 
                 (append (multiple-value-list (pixel image i j))
                         (list 255))))
         rgba-image)))
    (rgba-image image)))

