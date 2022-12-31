;; -*- Lisp -*-

(defpackage :storage-handlers
  (:use :cl :alexandria)
  (:export #:create-handler
           #:handler-get
           #:handler-put
           #:handler-length))

(in-package :storage-handlers)

(defclass storage-handler ()
  ((url :initarg :url :reader url)
   (flags :initarg :flags :reader flags)
   (length :reader handler-length)))

(defgeneric handler-get (handler offset length))
(defgeneric handler-put (handler offset length))

(defun handler-class (url)
  (ecase (puri:uri-scheme (puri:parse-uri url))
    ((nil :file) 'file-handler)
    ((:http :https) 'drakma-handler)))

(defun create-handler (url flags)
  (make-instance (handler-class url) :url url :flags flags))

(defclass drakma-handler (storage-handler)
  ((buf :reader buf)))

(defmethod initialize-instance :after ((handler drakma-handler) &key url)
  (multiple-value-bind (response status) (drakma:http-request url :force-binary t)
    (cond
      ((= status 200)
       (format t "; received ~A bytes~%" (length response))
       (setf (slot-value handler 'buf) response
             (slot-value handler 'length) (length response)))
      (t
       (error "could not retrieve ~A, HTTP status ~A" url status)))))

(defmethod handler-get ((handler drakma-handler) offset length)
  (subseq (buf handler) offset (+ offset length)))

(defclass file-handler (storage-handler)
  ((path :reader path)))
