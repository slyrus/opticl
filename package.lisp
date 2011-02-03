
(cl:defpackage :opticl
  (:use #:cl)
  (:export

   #:image

   #:1-bit-gray-image
   #:make-1-bit-gray-image
   #:1-bit-gray-pixel

   #:2-bit-gray-image
   #:make-2-bit-gray-image
   #:2-bit-gray-pixel

   #:4-bit-gray-image
   #:make-4-bit-gray-image
   #:4-bit-gray-pixel

   #:8-bit-gray-image
   #:make-8-bit-gray-image
   #:8-bit-gray-pixel

   #:16-bit-gray-image
   #:make-16-bit-gray-image
   #:16-bit-gray-pixel


   #:4-bit-rgb-image
   #:make-4-bit-rgb-image
   #:4-bit-rgb-pixel

   #:5-bit-rgb-image
   #:make-5-bit-rgb-image
   #:5-bit-rgb-pixel

   #:8-bit-rgb-image
   #:make-8-bit-rgb-image
   #:8-bit-rgb-pixel

   #:16-bit-rgb-image
   #:make-16-bit-rgb-image
   #:16-bit-rgb-pixel


   #:4-bit-rgba-image
   #:make-4-bit-rgba-image
   #:4-bit-rgba-pixel

   #:5-bit-rgba-image
   #:make-5-bit-rgba-image
   #:5-bit-rgba-pixel

   #:8-bit-rgba-image
   #:make-8-bit-rgba-image
   #:8-bit-rgba-pixel

   #:16-bit-rgba-image
   #:make-16-bit-rgba-image
   #:16-bit-rgba-pixel))
