
(defun read-jpeg-stream (stream)
  (declare (ignore stream))
  (warn "JPEG reading not supported on ABCL yet."))

(defun read-jpeg-file (pathname)
  (declare (ignore pathname))
  (warn "JPEG reading not supported on ABCL yet."))

(defun write-jpeg-stream (stream image)
  (declare (ignore stream image))
  (warn "JPEG writing not supported on ABCL yet."))

(defun write-jpeg-file (pathname image)
    (declare (ignore pathname image))
  (warn "JPEG writing not supported on ABCL yet."))
