;; -*- Lisp -*-

(in-package :a2-comm)

(define-condition timeout-while-trying-to-send (error) ())
(define-condition timeout-while-trying-to-receive (error) ())

(cl-gpiod:define-gpio apple2-rpi-io-gpio
  :chip-name "gpiochip0"
  :ports ((data-out :lines (5 11 9 10 22 27 17 4)
                    :direction :output)
          (data-in :lines (12 16 20 21 26 19 13 6)
                   :direction :input)
          (write-req :line 23
                     :event :falling-edge)
          (read-req :line 18
                    :event :falling-edge)
          (write-ack :line 25
                     :direction :output)
          (read-ack :line 24
                    :direction :output)))

(defun receive-byte (&optional hotp)
  (setf (write-ack) nil)
  (if hotp
      (loop while (write-req))
      (unless (cl-gpiod:wait-for-event-with-timeout 'write-req +apple2-timeout+)
        (when (write-req)
          (signal 'timeout-while-trying-to-receive))))
  (let ((byte (data-in)))
    (setf (write-ack) t)
    (loop while (not (write-req)))
    (setf (write-ack) nil)
    byte))

(defun send-byte (byte &optional hotp)
  (if hotp
      (loop while (read-req))
      (unless (cl-gpiod:wait-for-event-with-timeout 'read-req +apple2-timeout+)
        (signal 'timeout-while-trying-to-send)))
  (setf (data-out) byte)
  (setf (read-ack) nil)
  (loop while (not (read-req)))
  (setf (read-ack) t))

(defvar *chip*)

(defun open-gpio ()
  (when (boundp '*chip*)
    (gpiod:chip-close *chip*))
  (setf *chip* (cl-gpiod:open-chip apple2-rpi-io-gpio "ovomorph")))

