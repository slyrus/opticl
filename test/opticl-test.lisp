
(cl:in-package #:opticl-test)

(in-suite :opticl)

(defun test-image (filename)
  (reduce #'merge-pathnames (list filename "test/images/")
          :from-end t
          :initial-value (asdf:component-pathname
                          (asdf:find-system "opticl"))))

(defun output-image (filename)
  (reduce #'merge-pathnames (list filename "test/output/")
          :from-end t
          :initial-value (asdf:component-pathname
                          (asdf:find-system "opticl"))))

(ensure-directories-exist (output-image ""))

(defun flatten-array (arr)
  (make-array (reduce #'* (array-dimensions arr))
              :displaced-to arr
              :element-type (array-element-type arr)))

(defun array- (arr1 arr2)
  (let ((flat-arr1 (flatten-array arr1))
        (flat-arr2 (flatten-array arr2)))
    (map 'vector #'- flat-arr1 flat-arr2)))

(defun sum-of-element-wise-differences (arr1 arr2)
  (reduce #'+ (array- arr1 arr2)))


;;; pixel accessor tests

;;
;; grayscale images

;; 1-bit grayscale
(test 1-bit-gray-image-pixel
  (let ((img (make-1-bit-gray-image 32 32 :initial-element 1)))
    (is (= (pixel img 31 31)) 1)))

(test 1-bit-gray-image-typed-pixel
  (let ((img (make-1-bit-gray-image 32 32 :initial-element 1)))
    (declare (type 1-bit-gray-image img))
    (is (= (pixel img 31 31)) 1)))

;; 2-bit grayscale
(test 2-bit-gray-image-pixel
  (let ((img (make-2-bit-gray-image 32 32 :initial-element 3)))
    (is (= (pixel img 31 31) 3))))

(test 2-bit-gray-image-typed-pixel
  (let ((img (make-2-bit-gray-image 32 32 :initial-element 3)))
    (declare (type 2-bit-gray-image img))
    (is (= (pixel img 31 31) 3))))

;; 4-bit grayscale
(test 4-bit-gray-image-pixel
  (let ((img (make-4-bit-gray-image 32 32 :initial-element 15)))
    (is (= (pixel img 31 31) 15))))

(test 4-bit-gray-image-typed-pixel
  (let ((img (make-4-bit-gray-image 32 32 :initial-element 15)))
    (declare (type 4-bit-gray-image img))
    (is (= (pixel img 31 31) 15))))

;; 8-bit grayscale
(test 8-bit-gray-image-pixel
  (let ((img (make-8-bit-gray-image 32 32 :initial-element 255)))
    (is (= (pixel img 31 31) 255))))

(test 8-bit-gray-image-typed-pixel
  (let ((img (make-8-bit-gray-image 32 32 :initial-element 255)))
    (declare (type 8-bit-gray-image img))
    (is (= (pixel img 31 31) 255))))

;; 16-bit grayscale
(test 16-bit-gray-image-pixel
  (let ((img (make-16-bit-gray-image 32 32 :initial-element #xffff)))
    (is (= (pixel img 31 31) #xffff))))

(test 16-bit-gray-image-typed-pixel
  (let ((img (make-16-bit-gray-image 32 32 :initial-element #xffff)))
    (declare (type 16-bit-gray-image img))
    (is (= (pixel img 31 31) #xffff))))

;; 32-bit grayscale
(test 32-bit-gray-image-pixel
  (let ((img (make-32-bit-gray-image 32 32 :initial-element #xffffffff)))
    (is (= (pixel img 31 31) #xffffffff))))

(test 32-bit-gray-image-typed-pixel
  (let ((img (make-32-bit-gray-image 32 32 :initial-element #xffffffff)))
    (declare (type 32-bit-gray-image img))
    (is (= (pixel img 31 31) #xffffffff))))


;;
;; RGB images

;; 4-bit RGB
(test 4-bit-rgb-image-pixel
  (let ((img (make-4-bit-rgb-image 32 32 :initial-element 15)))
    (is (equalp (pixel img 31 31) (values 15 15 15)))))

(test 4-bit-rgb-image-typed-pixel
  (let ((img (make-4-bit-rgb-image 32 32 :initial-element 15)))
    (declare (type 4-bit-rgb-image img))
    (is (equalp (pixel img 31 31) 15))))

;; 8-bit RGB
(test 8-bit-rgb-image-pixel
  (let ((img (make-8-bit-rgb-image 32 32 :initial-element 255)))
    (is (equalp (pixel img 31 31) (values 255 255 255)))))

(test 8-bit-rgb-image-typed-pixel
  (let ((img (make-8-bit-rgb-image 32 32 :initial-element 255)))
    (declare (type 8-bit-rgb-image img))
    (is (equalp (pixel img 31 31) (values 255 255 255)))))

;; 16-bit RGB
(test 16-bit-rgb-image-pixel
  (let ((img (make-16-bit-rgb-image 32 32 :initial-element #xffff)))
    (is (equalp (pixel img 31 31) (values #xffff #xffff #xffff)))))

(test 16-bit-rgb-image-typed-pixel
  (let ((img (make-16-bit-rgb-image 32 32 :initial-element #xffff)))
    (declare (type 16-bit-rgb-image img))
    (is (equalp (pixel img 31 31) (values #xffff #xffff #xffff)))))

;; 32-bit RGB
(test 32-bit-rgb-image-pixel
  (let ((img (make-32-bit-rgb-image 32 32 :initial-element #xffffffff)))
    (is (equalp (pixel img 31 31) (values #xffffffff #xffffffff #xffffffff)))))

(test 32-bit-rgb-image-typed-pixel
  (let ((img (make-32-bit-rgb-image 32 32 :initial-element #xffffffff)))
    (declare (type 32-bit-rgb-image img))
    (is (equalp (pixel img 31 31) (values #xffffffff #xffffffff #xffffffff)))))
