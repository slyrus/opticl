opticl
A library for representing and processing images in Common Lisp (CL)

Cyrus Harmon <ch-lisp@bobobeach.com>, February 2011
See COPYRIGHT file for license details

* OVERVIEW

opticl is designed to be a high-performance, but relatively
lightweight, library for representing, processing, loading, and saving
2-dimensional pixel-based images. opticl aims to improve upon my first
attempt at an image processing library -- ch-image, and also borrows
some ideas from Matthieu Villeneuve's excellent imago image processing
library. Representing and processing images provides an excellent
illustration of the tradeoffs between generality, and complexity, on
the one hand, and simplicity and efficiency, on the other hand. All
other things being equal, one generall wants a simple sytem that is
both efficient and general enough to be suitable for use in a variety
of different contexts -- opticl aims to strike this balance and to be
both broadly applicable in different contexts and to provide a core
set of functionality that is of high-enough performance to be useful
in time-(and resource-)sensitive operations.

* INSTALLATION

NOTE: THIS DOESN'T WORK YET!!!
The easiest way (will be) to use Zachary Beane's fabulous quicklisp library:
(ql:quickload 'opticl)

* FOR THE IMPATIENT

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

If we time the (loop for i below...) using the time macro, with SBCL we see
the following:

    Evaluation took:
      0.006 seconds of real time
      0.005808 seconds of total run time (0.005807 user, 0.000001 system)
      100.00% CPU
      12,326,048 processor cycles
      0 bytes consed

Which shows that we're able to perform simple arithmetic operations on
each pixel of the image in 6 microseconds, and that we don't need to
cons to do so.

* IMAGE REPRESENTATION

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

* DEPENDENCIES

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

Retrospectiff must be installed by hand using a recent version from [github](https://github.com/slyrus/retrospectiff):

    git://github.com/slyrus/retrospectiff.git

Once opticl is on quicklisp, this step will be replaced by:

    (ql:quickload 'opticl)

* EXAMPLES

TBW
