;;; Copyright (c) 2011 Cyrus Harmon, All rights reserved.
;;; See COPYRIGHT file for details.

(in-package :opticl)

(defparameter *whitespace-chars* '(#\# #\Space #\Newline #\Tab #\Linefeed #\Return))

(defun skip-line (stream)
  (loop for c = (read-byte stream)
     until (find c (map 'vector #'char-code '(#\Newline #\Linefeed)))))

(defun read-until-whitespace (stream)
  (loop for c = (read-byte stream)
     until (find c (map 'vector #'char-code *whitespace-chars*))
     collect c))

(defun read-byte-skipping-whitespace (stream)
  (loop for c = (read-byte stream)
     while (find c (map 'vector #'char-code *whitespace-chars*))
     do (when (eq c (char-code #\#))
          (skip-line stream))
     finally (return c)))

(defun read-digit-chars (stream)
  (loop for c = (code-char (read-byte stream))
     while (digit-char-p c)
     collect c))

(defun read-number (stream)
  (let ((first-char (read-byte-skipping-whitespace stream)))
    (map 'string #'code-char
         (cons first-char (read-until-whitespace stream)))))


(defun read-8-bit-gray-pgm-data (stream height width)
  (let ((img (make-8-bit-gray-image height width)))    
    (loop for i below height
       do 
         (loop for j below width
            do (setf (pixel img i j)
                     (read-byte stream))))
    img))

(defun read-16-bit-gray-pgm-data (stream height width)
  (let ((img (make-8-bit-gray-image height width)))    
    (loop for i below height
       do 
         (loop for j below width
            do (setf (pixel img i j)
                     (+ (ash (read-byte stream) 8)
                        (read-byte stream)))))
    img))


(defun read-pgm-stream (stream)
  (let ((magic (make-array 2 :element-type '(unsigned-byte 8))))
    (read-sequence magic stream)
    (if (equalp magic #(80 53)) ;; P5 magic number
        (let ((width (parse-integer (read-number stream)))
              (height (parse-integer (read-number stream)))
              (maxgray (parse-integer (read-number stream))))
          (let ((img
                 (if (< maxgray 256)
                     (read-8-bit-gray-pgm-data stream height width)
                     (read-16-bit-gray-pgm-data stream height width))))
            img))
        (error "Invalid PGM Magic Number"))))

(defun read-pgm-file (pathname)
  (with-open-file (stream pathname :direction :input :element-type '(unsigned-byte 8))
    (read-pgm-stream stream)))

(defun write-pgm-stream (stream image)
  (error "not yet!"))

(defun write-pgm-file (pathname image)
  (with-open-file (stream pathname :direction :output
                          :if-exists :supersede
                          :element-type '(unsigned-byte 8))
    (write-pgm-stream stream image)
    (truename pathname)))
