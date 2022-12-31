;; -*- Lisp -*-

(in-package :nhacp)

(defvar *handlers* (make-array 255 :initial-element nil))

(defun select-free-index ()
  (loop for index below (length *handlers*)
        unless (aref *handlers* index)
          do (return index)
        finally (error "No free storage slot found")))

(define-handler storage-open (stream)
  (with-slots (index flags url-length) (binary-types:read-binary-record 'storage-open-request stream)
    (let* ((url (read-bytes url-length stream))
           (index (if (= index 255) (select-free-index) index))
           (handler (storage-handlers:create-handler url flags)))
      (setf (aref *handlers* index) handler)
      (make-response 'storage-loaded-response
                     nil
                     :index index
                     :length (storage-handlers:handler-length handler)))))

(define-handler storage-get (stream)
  (with-slots (index offset length) (binary-types:read-binary-record 'storage-get-request stream)
    (let* ((handler (aref *handlers* index))
           (buffer (storage-handlers:handler-get handler offset length)))
      (make-response 'data-buffer-response
                     buffer
                     :length (length buffer)))))

(define-handler storage-put (stream)
  (with-slots (index offset length) (binary-types:read-binary-record 'storage-get-request stream)
    (let ((handler (aref *handlers* index))
          (buffer (make-array length :element-type '(unsigned-byte 8))))
      ;; fixme error handling
      (read-sequence buffer stream)
      (storage-handlers:handler-put handler offset buffer)
      (make-response 'ok-response
                     nil))))

(define-handler get-date-time (stream)
  (multiple-value-bind (second minute hour day month year) (decode-universal-time (get-universal-time))
    (make-response 'date-time-response
                   nil
                   :date (format nil "~4,'0D~2,'0D~2,'0D" year month day)
                   :time (format nil "~4,'0D~2,'0D~2,'0D" hour minute second))))

(define-handler end-protocol (stream)
  (throw 'end-protocol nil))
