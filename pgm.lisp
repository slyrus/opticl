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

(defun read-number (stream)
  (let ((first-char (read-byte-skipping-whitespace stream)))
    (map 'string #'code-char
         (cons first-char (read-until-whitespace stream)))))

(defun %read-pgm-ascii-stream (stream)
  (let ((width (parse-integer (read-number stream)))
        (height (parse-integer (read-number stream)))
        (maxgray (parse-integer (read-number stream))))
    (if (< maxgray 256)
        (let ((img (make-8-bit-gray-image height width)))    
          (loop for i below height
             do 
             (loop for j below width
                do (setf (pixel img i j)
                         (read-number stream))))
          img)
        (let ((img (make-8-bit-gray-image height width)))    
          (loop for i below height
             do 
             (loop for j below width
                do (setf (pixel img i j)
                         (read-number stream))))
          img))))

(defun %read-pgm-binary-stream (stream)
  (let ((width (parse-integer (read-number stream)))
        (height (parse-integer (read-number stream)))
        (maxgray (parse-integer (read-number stream))))
    (if (< maxgray 256)
        (let ((img (make-8-bit-gray-image height width)))    
          (loop for i below height
             do 
             (loop for j below width
                do (setf (pixel img i j)
                         (read-byte stream))))
          img)
        (let ((img (make-8-bit-gray-image height width)))    
          (loop for i below height
             do 
             (loop for j below width
                do (setf (pixel img i j)
                         (+ (ash (read-byte stream) 8)
                            (read-byte stream)))))
          img))))

(defun read-pgm-stream (stream)
  (let ((magic (make-array 2 :element-type '(unsigned-byte 8))))
    (read-sequence magic stream)
    (cond ((equalp magic #(80 50)) ;; P2 magic number
           (%read-pgm-ascii-stream stream))
          ((equalp magic #(80 53)) ;; P5 magic number
           (%read-pgm-binary-stream stream))
          (t (error "Invalid PGM Magic Number")))))

(defun read-pgm-file (pathname)
  (with-open-file (stream pathname :direction :input :element-type '(unsigned-byte 8))
    (read-pgm-stream stream)))

(defun write-integer (int stream)
  (map nil (lambda (x) (write-byte (char-code x) stream)) (format nil "~D" int)))

(defun %write-pgm-ascii-stream (stream image)
  (map nil (lambda (x) (write-byte (char-code x) stream)) "P2")
  (write-byte (char-code #\Newline) stream)
  (with-image-bounds (height width)
      image
    (write-integer width stream)
    (write-byte (char-code #\Space) stream)
    (write-integer height stream)
    (write-byte (char-code #\Newline) stream)
    (typecase image
      (8-bit-gray-image
       (write-integer #xff stream))
      (16-bit-gray-image
       (write-integer #xffff stream)))
    (write-byte (char-code #\Newline) stream)
    (loop for i below height
       do (loop for j below width
             do 
               (write-integer (pixel image i j) stream)
               (unless (= j (1- width))
                 (write-byte (char-code #\Space) stream)))
         (write-byte (char-code #\Newline) stream))))

(defun %write-pgm-binary-stream (stream image)
  (map nil (lambda (x) (write-byte (char-code x) stream)) "P5")
  (write-byte (char-code #\Newline) stream)
  (with-image-bounds (height width)
      image
    (write-integer width stream)
    (write-byte (char-code #\Newline) stream)
    (write-integer height stream)
    (write-byte (char-code #\Newline) stream)
    (typecase image
      (8-bit-gray-image
       (locally
           (declare (type 8-bit-gray-image image))
         (write-integer #xff stream)
         (write-byte (char-code #\Newline) stream)
         (loop for i below height
            do (loop for j below width
                  do (write-byte (pixel image i j) stream)))))
      (16-bit-gray-image
       (locally
           (declare (type 16-bit-gray-image image))
         (write-integer #xffff stream)
         (write-byte (char-code #\Newline) stream)
         (loop for i below height
            do (loop for j below width
                  do (let ((val (pixel image i j)))
                       (write-byte (ash val -8) stream)
                       (write-byte (logand val #xff) stream)))))))))

(defun write-pgm-stream (stream image &key (binary t))
  (if binary
      (%write-pgm-binary-stream stream image)
      (%write-pgm-ascii-stream stream image)))

(defun write-pgm-file (pathname image
                       &key (binary t))
  (with-open-file (stream pathname :direction :output
                          :if-exists :supersede
                          :element-type '(unsigned-byte 8))
    (write-pgm-stream stream image :binary binary)
    (truename pathname)))
