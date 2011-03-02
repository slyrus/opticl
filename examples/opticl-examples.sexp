
((:p
  (:smarkup-metadata 
   (:copyright
    "Copyright 2011, Cyrus Harmon. See LICENSE file.")
   (:title "Opticl Examples")
   (:author "Cyrus L. Harmon"))
  (:html-metadata
   (:htmlcss "smarkup.css")
   (:htmlcss "reset.css")))
 
 (:h1 "Opticl Examples")
 
 (:h2 "Preliminaries")
 
 (:p "Let's make (and use) a Common Lisp package for exploring the opticl examples:")
 
 (:lisp
  #q{
  (defpackage #:opticl-examples
    (:use #:cl #:opticl))
  (in-package #:opticl-examples)
  })
 
 (:p "Second, we'll make some utility functions that we'll use for
  finding sample images and figuring out where to place output
  images:")

 (:lisp
  #q{
  (defun test-image (filename)
    (merge-pathnames filename
                     (asdf:component-pathname
                      (reduce #'asdf:find-component
                              '("opticl-test" "images")))))
  
  (defun output-image (filename)
    (reduce #'merge-pathnames (list filename "output/")
            :from-end t
            :initial-value (asdf:component-pathname
                            (asdf:find-system "opticl-test"))))
  })
 
 (:h2 "Making Some (Small) Images")
 
 (:p #q{First, a 8-bit grayscale image:})
 
 (:lisp #q{(defparameter *gray-image* (make-8-bit-gray-image 4 4))})
 
 (:p "Evaluating " (:code "*gray-image* " " yields:"))
 
 (:lisp #q{*gray-image*})
 
 (:p "we see that we have a 2-d array that contains the pixel information for the image.")
 
 (:p #q{Now, an 8-bit RGB image:})
 
 (:lisp #q{(defparameter *rgb-image* (make-8-bit-rgb-image 4 4))})
 
 (:p "Now, when we evalute " (:code "*rgb-image*") 
     " we see that " (:code "*rgb-image*") " contains a 3-d array:")
 
 (:lisp #q{*rgb-image*})
 
 (:h2 "Loading Images from Files")

 (:p "In this simple example, we'll load a TIFF image into the variable "
     (:code "*truck-image*") " and save it as a JPEG file:")

 (:lisp
  #q{
  (defparameter *truck-image* (read-tiff-file (test-image "truck.tiff")))

  (defparameter *truck-jpeg-file*
    (let ((out (output-image "truck.jpeg")))
      (write-jpeg-file out *truck-image*)))
  })

 (:image (:lisp-value #q{(namestring *truck-jpeg-file*)}))
 
 (:h2 "Resizing an image")

 (:p "To resize the truck image from above, we can do: ")
 
 (:lisp
  #q{
  (defparameter *resized-truck-image*
    (resize-image *truck-image* 200 300))
  
  (defparameter *resized-truck-jpeg-file*
    (let ((out (output-image "resized-truck.jpeg")))
      (write-jpeg-file out *resized-truck-image*)))
  })

 (:image (:lisp-value #q{(namestring *resized-truck-jpeg-file*)}))
 
 (:h2 "Drawing Circles")

 (:lisp
  #q{
  (defun test-circles ()
    (declare (optimize (speed 3) (safety 0)))
    (let ((height 480) (width 640))
      (let ((img (make-8-bit-rgb-image height width)))
        (declare (type 8-bit-rgb-image img))
        (fill-image img 20 20 90)
        (loop for i below 100
           do (let ((y (random height))
                    (x (random width))
                    (radius (random 100))
                    (r (random 256))
                    (g (random 256))
                    (b (random 256))
                    (fill (random 2)))
                (if (plusp fill)
                    (opticl::fill-circle img y x radius r g b)
                    (draw-circle img y x radius r g b))))
        img)))

  (defun write-circle-images ()
    (let ((img (test-circles)))
      (write-png-file "test/output/circles.png" img)))
  
  (defparameter *circles* (write-circle-images))
  })
 
 (:image
  (:lisp-value
   #q{(enough-namestring *circles*
                         (asdf:component-pathname (asdf:find-system :opticl-examples)))})))

