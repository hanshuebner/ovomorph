;; -*- Lisp -*-

(defpackage :shell
  (:use :cl :alexandria))

(in-package :shell)

(a2-server:define-command (shell 9)
  (format t "Shell started~%")
  ; (trace a2-comm:receive-byte a2-comm:send-byte a2-comm:byte-available-p flex:peek-byte pty)
  (let (reader)
    (unwind-protect
         (let* ((process (sb-ext:run-program (sb-ext:posix-getenv "SHELL") nil
                                             :wait nil
                                             :pty t))
                (pty (flex:make-flexi-stream (sb-ext:process-pty process))))
           (setf reader (bt:make-thread (lambda ()
                                          (catch 'done
                                            (loop while (sb-ext:process-alive-p process)
                                                  for byte = (read-byte pty nil)
                                                  if byte
                                                    do (format t "got $~2,'0X from PTY~%" byte)
                                                       (a2-comm:send-byte (logior byte #x80)))))
                                        :name "Shell Process PTY Reader"))
           (handler-case
               (loop while (sb-ext:process-alive-p process)
                     for byte = (a2-comm:receive-byte)
                     if (= byte 0)
                       do (sb-ext:process-kill process 15)
                     else
                       do (format t "got $~2,'0X from Apple~%" byte)
                          (write-byte byte pty)
                          (finish-output pty))
             (error (e)
               (format t "Error ~A ignored~%" e)
               (finish-output))))
      (when reader
        (ignore-errors (bt:interrupt-thread reader
                                            (lambda ()
                                              (throw 'done nil)))))))
  (a2-comm:send-byte 0)
  (format t "Shell done~%"))
