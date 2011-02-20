
# Efficient Access to Pixel Information in Images

We want a way to efficiently (using few processor cycles and minimally
consing) access information about individual pixes in images. Multiple
values allow for a non-consing way to get and set more than one value
at a time using the lisp implementation's argument passing and value
returning facilities without having to explicitly place values in or
retrieve values from a list.

Reading pixel values is pretty straightforward:

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

This handles both single-channel (grayscale) and multi-channel
(RGB and RGBA) pixels, returning the number of values as
appropriate.

Setting pixels, on the other hand, is a bit tricker. We want a form
that allows us to (setf (pixel img y x) ...) and take the number of
values as appropriate for the particular image, but we also want this
setting to be non-consing and efficient. CL has a define-setf-expander
that can be used for just such a thing. Turns out it's fairly tricky
to get this right, so I have included my intermediate attempts,
followed by the final version.

## My original define-setf-expander approach

    (defconstant +max-image-channels+ 4)

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

##  Robert Strandh's with-image macro:

    (defmacro with-image ((image-var height width &optional (depth 1)) &body body &environment env)
      (let* ((old-info (if (eq (macroexpand-1 'image-info env) 'image-info)
                           '()
                           (macroexpand-1 'image-info env)))
             (new-info (cons (list image-var height width depth) old-info)))
        `(symbol-macrolet ((image-info ,new-info))
           ,@body)))

    (define-setf-expander pixel** (image-var y x &environment env)
      (let ((arity (fourth (assoc image-var (macroexpand-1 'image-info env))))
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
                      `(values ,@(loop for i from 0
                                       for store in stores
                                       collect `(aref ,image-var ,temp-y ,temp-x ,i))))))))


## An improved setf-expander



Questions:

 * Should grayscale images have be 3-dimensional arrays with a 3-rd
   dimension of 1 instead of 2-d images? It would simplify some code
   in that we would know that there would always be three indices for
   arrays.

 * Should we use the image-macro for establishing compile-time
   information about arrays?

 * What about cltl2 declaration-information? But for the lack of
   portability, we could use SBCL's (sb-cltl2:declaration-information
   'declaration env) from the setf-expander to get hints about the
   compile-time type of the array and react accordingly.

 
