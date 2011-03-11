
(in-package :opticl-color)

(defun whitespace-p (char)
  (member char '(#\Space #\Tab)))

(defun trim-leading-whitespace (str)
  (let ((pos (position-if-not #'whitespace-p str)))
    (subseq str pos)))

(macrolet ((frob-colors ()
             (let ((file (merge-pathnames
                          "rgb.txt"
                          (asdf:component-pathname
                           (asdf:find-system "opticl")))))
               (with-open-file (stream file)
                 `(progn
                    ,@(loop for r = (read stream nil nil)
                         while r 
                         collect
                           (let ((g (read stream))
                                 (b (read stream))
                                 (color 
                                  (concatenate 'string
                                               "*"
                                               (substitute-if
                                                #\- #'whitespace-p
                                                (trim-leading-whitespace
                                                 (read-line stream)))
                                               "*")))
                             (let ((sym (read-from-string color)))
                               `(progn
                                  (defparameter ,sym (list ,r ,g ,b))
                                  (export ',sym))))))))))
  (frob-colors))
