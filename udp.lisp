;; -*- Lisp -*-

(in-package :a2-comm)

(defconstant +send-port+ 22129)
(defconstant +receive-port+ 22130)

(defvar *server-thread*)
(defvar *send-socket*)
(defvar *receive-queue* (lparallel.vector-queue:make-vector-queue 256))

(defun receive-byte (&optional hotp)
  (declare (ignore hotp))
  (lparallel.queue:pop-queue *receive-queue*))

(defun byte-available-p ()
  (not (lparallel.queue:queue-empty-p *receive-queue*)))

(defun send-byte (byte &optional hotp)
  (declare (ignore hotp))
  (let ((buf (make-array 1 :initial-element byte :element-type '(unsigned-byte 8))))
    (handler-case
        (usocket:socket-send *send-socket* buf 1)
      (error (e)
        (format t "cannot send byte to Apple II: ~A~%" e)))))

(defun receiver ()
  (catch 'done
    (usocket:socket-server nil +receive-port+
                           (lambda (buf)
                             (loop for byte across buf
                                   do (lparallel.queue:push-queue byte *receive-queue*)))
                           nil
                           :protocol :datagram)))

(defun stop-receiver ()
    (when (boundp '*server-thread*)
      (unwind-protect
           (bt:interrupt-thread *server-thread*
                                (lambda ()
                                  (throw 'done nil)))
        (makunbound '*server-thread*))))


(defun open-gpio ()
  (stop-receiver)
  (setf *server-thread* (bt:make-thread 'receiver
                                        :name "UDP GPIO receiver")
        *send-socket* (usocket:socket-connect "localhost" +send-port+ :protocol :datagram)))
