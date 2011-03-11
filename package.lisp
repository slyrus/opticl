;;; Copyright (c) 2011 Cyrus Harmon, All rights reserved.
;;; See COPYRIGHT file for details.

(cl:defpackage :opticl
  (:use #:cl)
  (:export

   #:image
   #:pixel
   #:pixel*

   #:1-bit-gray-image #:make-1-bit-gray-image
   #:2-bit-gray-image #:make-2-bit-gray-image
   #:4-bit-gray-image #:make-4-bit-gray-image
   #:8-bit-gray-image #:make-8-bit-gray-image
   #:16-bit-gray-image #:make-16-bit-gray-image

   #:4-bit-rgb-image #:make-4-bit-rgb-image
   #:8-bit-rgb-image #:make-8-bit-rgb-image
   #:16-bit-rgb-image #:make-16-bit-rgb-image

   #:4-bit-rgba-image #:make-4-bit-rgba-image
   #:8-bit-rgba-image #:make-8-bit-rgba-image
   #:16-bit-rgba-image #:make-16-bit-rgba-image

   ;;
   #:constrain
   #:with-image-bounds
   #:pixel-in-bounds

   #:copy-image
   #:crop-image
   #:trim-image
   
   #:map-array

   #:resize-image
   #:transform-image
   #:make-affine-transformation

   ;; convolution and friends
   #:discrete-convolve
   #:sharpen-image
   #:blur-image

   ;; Drawing primitives
   #:fill-image
   #:fill-image*
   #:draw-circle
   #:draw-circle*
   #:fill-circle
   #:fill-circle*
   #:draw-rectangle
   #:draw-rectangle*
   #:horizontal-line
   #:horizontal-line*
   #:vertical-line
   #:vertical-line*
   #:draw-line
   #:draw-line*
   #:draw-triangle
   #:draw-triangle*

   ;; Various Image Operations
   #:make-gamma-curve-lookup-table
   #:apply-gamma-curve-lookup-table
   #:apply-gamma

   ;; I/O
   #:read-jpeg-file
   #:read-jpeg-stream
   #:write-jpeg-file
   #:write-jpeg-stream

   #:read-png-file
   #:read-png-stream
   #:write-png-file
   #:write-png-stream

   #:read-tiff-file
   #:read-tiff-stream
   #:write-tiff-file
   #:write-tiff-stream
   
   #:read-pnm-stream
   #:read-pnm-file

   #:read-pbm-stream
   #:read-pbm-file
   #:write-pbm-stream
   #:write-pbm-file

   #:read-pgm-stream
   #:read-pgm-file
   #:write-pgm-stream
   #:write-pgm-file
   
   #:read-ppm-stream
   #:read-ppm-file
   #:write-ppm-stream
   #:write-ppm-file

   #:read-image-file
   #:read-image-stream
   #:write-image-file
   #:write-image-stream

   ;; conversion
   #:convert-image-to-grayscale
   #:convert-image-to-grayscale-luminance
   #:convert-image-to-rgb
   #:convert-image-to-rgba

   ))

(in-package :opticl)

(defun ignore-warning (condition)
  (declare (ignore condition))
  (muffle-warning))

(handler-bind ((warning #'ignore-warning))
  (defpackage :opticl-color
    (:use :cl)))

#+sbcl
(require :sb-cltl2)

(defpackage :opticl-cltl2
  #+sbcl (:import-from :sb-cltl2 :variable-information)
  #+ccl (:import-from :ccl :variable-information)
  #+(or sbcl ccl) (:export :variable-information))

