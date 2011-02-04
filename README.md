* opticl: A library for representing and processing images in Common Lisp (CL)

By Cyrus Harmon <ch-lisp@bobobeach.com>, February 2011. See COPYRIGHT
file for license details.

* Overview

opticl is designed to be a high-performance, but relatively
lightweight, library for representing, processing, loading, and saving
2-dimensional pixel-based images. opticl aims to improve upon my first
attempt at an image processing library -- ch-image, and also borrows
some ideas from Matthieu Villeneuve's excellent imago image processing
library. Representing and processing images provides an excellent
illustration of the tradeoffs between generality, and complexity, on
the one hand, and simplicity and efficiency, on the other hand. All
other things being equal, one generally wants a simple system that is
both efficient and general enough to be suitable for use in a variety
of different contexts -- opticl aims to strike this balance and to be
both broadly applicable in different contexts and to provide a core
set of functionality that is of high-enough performance to be useful
in time-(and resource-)sensitive operations.

* Installation

NOTE: THIS DOESN'T WORK YET!!!  The easiest way (will be) to use
Zachary Beane's fabulous quicklisp library: `(ql:quickload 'opticl)`

* For the Impatient

For a quick example, let's load an image (of a truck) from a JPEG
file, invert the red channel and save the image back out to another
jpeg file:

    (defpackage #:impatient (:use #:cl #:opticl))
    (in-package #:impatient)

    (let ((img (read-jpeg-file "test/images/truck.jpeg")))
      (destructuring-bind (height width channels)
          (array-dimensions img)
        (declare (ignore channels))
        (loop for i below height
           do (loop for j below width 
                 do 
                 (multiple-value-bind (r g b)
                     (8-bit-rgb-pixel img i j)
                   (declare (type (unsigned-byte 8) r g b))
                   (setf (8-bit-rgb-pixel img i j)
                         (values (- 255 r) g b))))))
      (write-jpeg-file "test/output/inv-r-truck.jpeg" img))

If we time the `(loop for i below...)` using the time macro, with SBCL we see
the following:

    Evaluation took:
      0.009 seconds of real time
      0.009671 seconds of total run time (0.009663 user, 0.000008 system)
      111.11% CPU
      20,533,416 processor cycles
      0 bytes consed

Which shows that we're able to perform simple arithmetic operations on
each pixel of the image in 9 milliseconds, and that we don't need to
cons to do so.

* Image Representation

In ch-image, images were represented by a set of CLOS classes which,
in turn, either extended or wrapped classes from the CLEM
matrix-processing library. The idea was that CLEM could do the heavy
lifting and ch-image could take advantage of CLEM's relatively
efficient routines for storing arrayed sets of 2-dimensional
numbers. This worked reasonably well, and allowed for ch-image to have
a great variety of, at least conceptual, image types, such as various
sizes of RGB and grayscale images, multichannel images, floating point
images, binary images, etc..., but this approach had to fundamental
costs. First, it required that client programs wishing to use ch-image
use CLEM as well -- and CLEM brings along a host of other things that
may not be desired by the image-library-using programmer. Second, and
more problematic, it relied on CLEM's facilities for accessing image
data, or digging deeply into CLEM data structures to get access to the
underlying data, which seems to be missing the point.

So... I've taken a different approach with opticl, which is to largely
eschew CLOS classes and to provide the image data directly as native
CL arrays. Clearly, some measure of abstraction can be useful to
insulate programmers from subsequent changes in the implementation,
but this abstraction should be kept to a minumum and should not get in
the way of programmers seeking to use the data. Therefore, the
fundamental data structure of opticl is the CL array, but the API to
create and access the data in these arrays is a set of functions that
are used to make images and to get and set the data in the
images. These functions are implemented as non-generic functions,
which can be inlined (with a sufficiently smart compiler) for
efficient access to image data. To date, opticl has only been tested
on SBCL, and, conversely, has been designed to exploit the
performance-enchancing characteristics of the SBCL compiler, such as
efficient access to specialized arrays (given proper type
declarations). opticl contains CL types (not classes) and the core
functions for creating and accessing and setting pixel values use
these type declarations to enable SBCL to generate relatively
efficient code for accessing image data.

** Multi-dimensional Arrays

Common Lisp's multidimensional provide some attractive qualities for
representing images. At the core, it is desirable to have a
representation that lends itself to efficient operations -- many
languages offer high performance one-dimensional array access, and
some offer efficient access to multidimensional arrays. However,
merely the bytes that comprise the underlying array may not be
sufficient for one to intelligently use the array. But the bytes that
make up the image are only part of the story, the other critical
pieces are the data that describes the bytes in those arrays, the
dimensions of the image, the number of image channels, etc... In
ch-image I used CLOS classes for this data and for holding a reference
to the underlying pixels themselves. Fortunately, CL's array type
itself enables us to store this metadata directly in a
multidimensional array. We define a mapping between various image
types and various specialized CL array types, such that, for instance,
an 8-bit RGB array is represented by the type `(SIMPLE-ARRAY
(UNSIGNED-BYTE 8) (* * 3))`. Any 3-dimensional simple-array with a
third dimension of size 3 and an element-type of `(unsigned-byte 8)`
will satisfy the conditions of being an `8-bit-rgb-image`.

This enables both opticl code and user code to infer the dimensions
and the kind of pixels represented in a(n appropriate) CL array that
hapepns to be on optiicl `image`. This, in turn, allows for both
opticl code and user code to use type declarations to enable the
compiler to generate high-performance code for processing images. It
is chiefly this facility that distinguishes opticl from other CL image
processing libraries such as ch-image and imago.

** Multiple Values

Another facility afforded by CL, is the notion of multiple values. If
one wants to represent a pixel of an 8-bit RGB image, and to perform
an operation on the individual color values of this pixel, one is
presented with a number of alternatives. Without using
multiple-values, one can treat the pixel as a single 24-bit unsigned
integer, knowing which bits correspond to the red, green and blue
channels; one can get the values as a list of three 8-bit integers; or
one can rely on reader/writer functions. Each of these alternatives
has some drawbacks.

The 24-bit unsigned integer approach is relatively clean, but requires
that user code unpack the image into it's resepective components. Easy
enough to do, but we just lost two things. First, the image would now
be represented as an array of unsigned 24-bit integers -- or in the
case of an RGBA image, unsigned 32-bit integers. How would one
distinguish this from a 32-bit grayscale image? One would need
additional information. Second, one would be relying on either user
code or library-provided facilities for unpacking the color
information. It is my belief that the compiler is going to do at least
as good of a job as user code in pulling those values out of an
additional array dimension than user or library code would. On the
other hand, uisng a list or reader/writer functions would likely
involve heap-allocation of data structures to store this information.

CL offers a facility that has the potential to alleviate these issues,
which is `multiple-values`. This allows us to return multiple (perhaps
stack-allocated) values from a function and for us to to efficiently
update the values in multiple places using `setf`. Furthermore, it
allows for a unified treatment of grayscale and RGB pixels as a
grayscale pixel is just a single value, while an RGB pixel is
represented by multiple values, as opposed to treating grayscale
values as an integer and RGB values as a list of integers. All of this
would just be theoretical nazel-gaving if the implementations didn't
take advantage of the features of multiple values to provide efficient
compiled implementations of code that uses these
features. Fortunately, SBCL's implementation of multiple-values allows
us to define (possibly inline) reader and writer functions that can
access the pixel and color-value data efficiently and without
allocating additional memory on the heap (consing).

The tradeoff in this approach is that doing so requires that we know
what kind of image with which are dealing. If we have an 8-bit RGB
image, we can use the `8-bit-rgb-pixel` and `(setf 8-bit-rgb-pixel)`
functions to read and write, respectively, pixel data. Other reader
and writer functions are provided for the other matrix types.

It is the representation of image data as native CL arrays and the
efficient performance of these reader and writer functions that offer
the hope that opticl can serve as a general purpose image processing
library suitable for use by a wide variety of CL programs.

* Dependencies

While opticl is designed to have minimal dependencies, I have decided
that it is better to use existing libraries, where possible,
especially for file I/O of various formats. In ch-image, I tried to
make the file I/O sections optional dependencies, but this proved
merely to sow confusion into the minds of the user. With the advent of
quicklisp, dependencies on libraries that are in quicklisp are much
less painful (for the quicklisp user anyway) than the used to be.

  retrospectiff (new version -- as of??)
   com.gigamonkeys.binary-data (also known as monkeylib-binary-data)
    alexandria
   ieee-floats
  zpng
   salza2
  png-read
   iterate
   chipz
   babel
  cl-jpeg

To install the dependencies using quicklisp:

    (ql:quickload 'zpng 'png-read 'cl-jpeg)

Retrospectiff must be installed by hand using a recent version from
[github](https://github.com/slyrus/retrospectiff):

    git://github.com/slyrus/retrospectiff.git

Once opticl is on quicklisp, this step will be replaced by:

    (ql:quickload 'opticl)

* Examples

TBW
