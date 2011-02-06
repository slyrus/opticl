;;; Copyright (c) 2011 Cyrus Harmon, All rights reserved.
;;; See COPYRIGHT file for details.

(cl:defpackage :opticl
  (:use #:cl)
  (:export

   #:image
   #:pixel

   #:1-bit-gray-image
   #:make-1-bit-gray-image
   #:pixel/1-bit-gray-image #:pixel/1-bit-gray-image

   #:2-bit-gray-image
   #:make-2-bit-gray-image
   #:pixel/2-bit-gray-image #:pixel/2-bit-gray-image

   #:4-bit-gray-image
   #:make-4-bit-gray-image
   #:pixel/4-bit-gray-image #:pixel/4-bit-gray-image

   #:8-bit-gray-image
   #:make-8-bit-gray-image
   #:pixel/8-bit-gray-image #:pixel/8-bit-gray-image

   #:16-bit-gray-image
   #:make-16-bit-gray-image
   #:pixel/16-bit-gray-image #:pixel/16-bit-gray-image


   #:4-bit-rgb-image
   #:make-4-bit-rgb-image
   #:pixel/4-bit-rgb-image #:pixel/4-bit-rgb-image

   #:5-bit-rgb-image
   #:make-5-bit-rgb-image
   #:pixel/5-bit-rgb-image #:pixel/5-bit-rgb-image

   #:8-bit-rgb-image
   #:make-8-bit-rgb-image
   #:pixel/8-bit-rgb-image #:pixel/8-bit-rgb-image

   #:16-bit-rgb-image
   #:make-16-bit-rgb-image
   #:pixel/16-bit-rgb-image #:pixel/16-bit-rgb-image


   #:4-bit-rgba-image
   #:make-4-bit-rgba-image
   #:pixel/4-bit-rgba-image #:pixel/4-bit-rgba-image

   #:5-bit-rgba-image
   #:make-5-bit-rgba-image
   #:pixel/5-bit-rgba-image #:pixel/5-bit-rgba-image

   #:8-bit-rgba-image
   #:make-8-bit-rgba-image
   #:pixel/8-bit-rgba-image #:pixel/8-bit-rgba-image

   #:16-bit-rgba-image
   #:make-16-bit-rgba-image
   #:pixel/16-bit-rgba-image #:pixel/16-bit-rgba-image
   
   ;; Drawing primitives
   #:fill-image
   #:draw-circle
   #:fill-circle
   
   ;; I/O
   #:read-jpeg-file
   #:read-jpeg-stream
   #:write-jpeg-file
   #:write-jpeg-stream

   #:read-png-file
   #:read-png-stream

   #:read-tiff-file
   #:read-tiff-stream
   ))
