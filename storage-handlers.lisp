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
(defgeneric handler-put (handler offset buffer))

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

(defun make-pathname-from-url (url)
  (when (ppcre:scan "^file://[^/]" url)
    (error "cannot handle file URL with host component"))
  (let ((input-pathname (pathname (puri:uri-path (puri:parse-uri url)))))
    (when (member :up (pathname-directory input-pathname))
      (error "cannot go up in directory hierarchy"))
    (merge-pathnames (make-pathname :name (pathname-name input-pathname)
                                    :type (pathname-type input-pathname)
                                    :directory (when-let ((directory (pathname-directory input-pathname)))
                                                 (cons :relative (rest directory)))))))

(defun determine-file-size (path)
  (with-open-file (file path :if-does-not-exist nil)
    (when file
      (file-length file))))

(defmethod initialize-instance :after ((handler file-handler) &key url)
  (with-slots (path length) handler
    (setf path (make-pathname-from-url url)
          length (or (determine-file-size path) 0))))

(defmethod handler-get ((handler file-handler) offset length)
  (with-open-file (file (path handler) :element-type '(unsigned-byte 8))
    (unless (file-position file offset)
      (error "cannot position to offset ~A of file ~A" offset (path handler)))
    (let* ((buf (make-array length :element-type '(unsigned-byte 8)))
           (position (read-sequence buf file)))
      (unless (= position length)
        (error "cannot read ~A bytes from file ~A" length (path handler)))
      buf)))

(defmethod handler-put ((handler file-handler) offset buffer)
  (with-open-file (file (path handler) :element-type '(unsigned-byte 8) :direction :output :if-exists :overwrite)
    (unless (file-position file offset)
      (error "cannot position to offset ~A of file ~A" offset (path handler)))
    (write-sequence buffer file)))
