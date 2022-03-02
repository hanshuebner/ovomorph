;; -*- Lisp -*-

(defpackage :disk
  (:use :cl :alexandria))

(in-package :disk)

;; Disk I/O

(defconstant +block-size+ 512)

(defvar *drives* (make-hash-table))

(defun close-all ()
  (loop for file being the hash-values of *drives*
        do (close file))
  (setf *drives* (make-hash-table)))

(defun drive-file (drive)
  (make-pathname :name (format nil "apple2-drive-~A" drive) :type "img"))

(defun drive-stream (drive)
  (or (gethash drive *drives*)
      (setf (gethash drive *drives*)
            (open (drive-file drive)
                  :element-type '(unsigned-byte 8)
                  :direction :io
                  :if-exists :overwrite ))))

(defmacro with-drive-and-block-number ((drive block-number) &body body)
  `(let* ((block-low (a2-comm:receive-byte))
          (block-high (a2-comm:receive-byte))
          (,drive (ash (a2-comm:receive-byte) -7))
          (,block-number (logior block-low (ash block-high 8))))
     ,@body))

(a2-server:define-command (read-block 1)
  (with-drive-and-block-number (drive block-number)
    (a2-server:log "Read drive ~D block ~D" drive block-number)
    (file-position (drive-stream drive) (* block-number +block-size+))
    (let ((buf (make-array +block-size+ :element-type '(unsigned-byte 8))))
      (read-sequence buf (drive-stream drive))
      (a2-comm:send-bytes buf))))

(a2-server:define-command (write-block 2)
  (with-drive-and-block-number (drive block-number)
    (a2-server:log "Write drive ~D block ~D" drive block-number)
    (let ((buf (a2-comm:receive-bytes +block-size+))
          (stream (drive-stream drive)))
      (file-position stream (* block-number +block-size+))
      (write-sequence buf stream)
      (finish-output stream))))

