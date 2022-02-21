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
   #:send-bytes))

(in-package :a2-comm)

(defconstant +apple2-timeout+ 1)

(cl-gpiod:define-gpio apple2-rpi-io-gpio
  :chip-name "gpiochip0"
  :ports ((data-out :lines (5 11 9 10 22 27 17 4)
                    :direction :output)
          (data-in :lines (12 16 20 21 26 19 13 6)
                   :direction :input)
          (in-write :line 23
                    :event :falling-edge
                    :flags (:active-low))
          (in-read :line 18
                   :event :falling-edge
                   :flags (:active-low))
          (out-read :line 25
                    :direction :output)
          (out-write :line 24
                     :direction :output)))

(defun receive-byte (&optional hotp)
  (setf (out-read) nil)
  (if hotp
      (loop while (in-write))
      (unless (cl-gpiod:wait-for-event-with-timeout 'in-write +apple2-timeout+)
        (throw 'timeout-while-trying-to-receive nil)))
  (let ((byte (data-in)))
    (setf (out-read) t)
    (loop while (not (in-write)))
    byte))

(defun send-byte (byte &optional hotp)
  (when (not (in-write))
    (throw 'incoming-byte-while-writing nil))
  (if hotp
      (loop while (in-read))
      (unless (cl-gpiod:wait-for-event-with-timeout 'in-read +apple2-timeout+)
        (throw 'timeout-while-trying-to-send nil)))
  (setf (data-out) byte)
  (setf (out-write) nil)
  (loop while (not (in-read)))
  (setf (out-write) t))

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

(defvar *chip*)

(defun open-gpio ()
  (when (boundp '*chip*)
    (gpiod:chip-close *chip*))
  (setf *chip* (cl-gpiod:open-chip apple2-rpi-io-gpio "a2-server")))

