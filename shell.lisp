;; -*- Lisp -*-

(defpackage :shell
  (:use :cl :alexandria))

(in-package :shell)

(defconstant +block-size+ 256)

(defun process-terminal-output (buf len)
  (flex:with-output-to-sequence (out)
    (loop for i below len
          do (write-byte (logior (aref buf i) #x80) out))))

(a2-server:define-command (shell 9)
  (format t "Shell started~%")
  ; (trace a2-comm:receive-byte a2-comm:send-byte a2-comm:byte-available-p flex:peek-byte pty)
  (let (reader)
    (unwind-protect
         (let* ((process (sb-ext:run-program (sb-ext:posix-getenv "SHELL") nil
                                             :wait nil
                                             :environment '("USER=hans"
                                                            "TERM=apple2e-p"
                                                            "HOME=/home/hans"
                                                            "SHELL=/bin/zsh")
                                             :pty t))
                (pty (sb-ext:process-pty process))
                (fd (sb-sys:fd-stream-fd pty)))
           (setf reader (bt:make-thread (lambda ()
                                          (catch 'done
                                            (loop with buf = (make-array +block-size+ :element-type '(unsigned-byte 8))
                                                  while (sb-ext:process-alive-p process)
                                                  for len = (sb-sys:with-pinned-objects (buf)
                                                              (sb-unix:unix-read fd (sb-sys:vector-sap buf) +block-size+))
                                                  if (plusp len)
                                                    do (format t "got ~D bytes from PTY~%" len)
                                                       (a2-comm:send-bytes (process-terminal-output buf len)))))
                                        :name "Shell Process PTY Reader"))
           (handler-case
               (loop while (sb-ext:process-alive-p process)
                     for byte = (a2-comm:receive-byte)
                     if (= byte 0)
                       do (sb-ext:process-kill process 15)
                     else
                       do (format t "got $~2,'0X from Apple~%" byte)
                          (let ((buf (make-array 1 :element-type '(unsigned-byte 8) :initial-element byte)))
                            (sb-sys:with-pinned-objects (buf)
                              (sb-unix:unix-write fd (sb-sys:vector-sap buf) 0 1))))
             (error (e)
               (format t "Error ~A ignored~%" e)
               (finish-output))))
      (when reader
        (ignore-errors (bt:interrupt-thread reader
                                            (lambda ()
                                              (throw 'done nil)))))))
  (a2-comm:send-byte 0)
  (format t "Shell done~%"))
