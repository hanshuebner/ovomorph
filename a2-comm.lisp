;; -*- Lisp -*-

(defpackage :a2-comm
  (:use :cl :alexandria)
  (:export
   #:open-gpio
   #:receive-byte
   #:send-byte
   #:receive-string
   #:send-string
   #:receive-bytes
   #:send-bytes
   #:byte-available-p))

(in-package :a2-comm)

(defconstant +apple2-timeout+ 1)

(defun receive-string ()
  (with-output-to-string (*standard-output*)
    (loop
      (let ((byte (receive-byte)))
        (when (zerop byte)
          (return))
        (princ (code-char byte))))))

(defun send-string (s)
  (loop for char across s
        do (send-byte (char-code char)))
  (send-byte 0))

(defun receive-bytes (count)
  (flex:with-output-to-sequence (buf)
    (dotimes (i count)
      (write-byte (receive-byte) buf))))

(defun send-bytes (buf)
  (loop for byte across buf
        do (send-byte byte)))

