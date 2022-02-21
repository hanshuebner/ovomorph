;; -*- Lisp -*-

(defpackage :a2-server
  (:use :cl :alexandria)
  (:shadow :log)
  (:export
   #:define-command
   #:log))

(in-package :a2-server)

(defvar *commands* (make-array 256 :initial-element nil :element-type 'symbol))

(defmacro define-command ((name command-number) &body body)
  `(progn
     (setf (aref *commands* ,command-number) ',name)
     (defun ,name () ,@body)))

(defun serve ()
  (a2-comm:open-gpio)
  ;; Initialize communications
  (a2-comm:send-byte 0)
  (loop
    (let* ((byte (a2-comm:receive-byte))
           (command (nth byte *commands*))) 
      (if (fboundp command)
          (funcall command)
          (warn "received command byte ~A (~A~) which is not recognized or implemented" byte command)))))

(defun log (s &rest args)
  (apply #'format s args)
  (terpri))

(define-command (reset 0)
  (log "Reset request")
  (a2-comm:send-byte 0))

