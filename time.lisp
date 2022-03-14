;; -*- Lisp -*-

(defpackage :time
  (:use :cl :alexandria))

(in-package :time)

;; getTime

(defun universal-to-prodos-time (universal-time)
  (multiple-value-bind (sec min hour day month year)
      (decode-universal-time universal-time)
    (declare (ignore sec))
    (let ((prodos-time 0))
      (setf (ldb (byte 8 24) prodos-time) hour
            (ldb (byte 8 16) prodos-time) min
            (ldb (byte 7 9) prodos-time) (mod year 100)
            (ldb (byte 4 5) prodos-time) month
            (ldb (byte 5 0) prodos-time) day)
      prodos-time)))

(defun prodos-to-universal-time (prodos-time)
  (encode-universal-time 0
                         (ldb (byte 8 16) prodos-time)
                         (ldb (byte 8 24) prodos-time)
                         (ldb (byte 5 0) prodos-time)
                         (ldb (byte 4 5) prodos-time)
                         (let ((year (ldb (byte 7 9) prodos-time)))
                           (+ year
                              (if (< year 76)
                                  2000
                                  1900)))))

(ovomorph:define-command (get-time 3)
  (let ((prodos-time (universal-to-prodos-time (get-universal-time))))
    (a2-comm:send-byte (ldb (byte 8 0) prodos-time))
    (a2-comm:send-byte (ldb (byte 8 8) prodos-time))
    (a2-comm:send-byte (ldb (byte 8 16) prodos-time))
    (a2-comm:send-byte (ldb (byte 8 24) prodos-time))))
