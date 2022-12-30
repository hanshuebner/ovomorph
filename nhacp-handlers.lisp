;; -*- Lisp -*-

(in-package :nhacp)

(define-handler storage-open (stream)
  (with-slots (index url-length) (binary-types:read-binary-record 'storage-http-get-request stream)
    (let ((url (read-bytes url-length stream)))
      (format t "; request URL ~A~%" url)
      (handler-case
          (multiple-value-bind (response status) (drakma:http-request url :force-binary t)
            (cond
              ((= status 200)
               (format t "; received ~A bytes~%" (length response))
               (setf (aref *buffers* index) response)
               (make-response 'storage-loaded-response
                              nil
                              :length (length response)))
              (t
               (make-error-response  "could not retrieve, HTTP status ~A~%" status))))
        (error (e)
          (make-error-response "failed to get URL: ~A" e))))))

#+(or)
(define-handler storage-load-file (stream)
  (with-slots (index filename-length) (binary-types:read-binary-record 'storage-load-file-request stream)
    (let ((pathname (read-bytes filename-length stream)))
      (format t "; pathname ~A~%" pathname)
      (handler-case
          (with-buffer (buffer index)
            (setf buffer (read-file-into-byte-vector pathname))
            (make-response 'storage-loaded-response
                           nil
                           :length (length buffer)))
        (error (e)
          (make-error-response "failed to get URL: ~A" e))))))

(define-handler storage-get (stream)
  (with-slots (index offset length) (binary-types:read-binary-record 'storage-get-request stream)
    (with-buffer (buffer index)
      (let ((end (+ offset length)))
        (auto-extend-buffer index end)
        (format t "; index ~A total ~A start ~A end ~A~%" index (length buffer) offset end)
        (make-response 'data-buffer-response
                       (subseq buffer offset end)
                        :length (length buffer))))))

(define-handler storage-put (stream)
  (with-slots (index offset length) (binary-types:read-binary-record 'storage-get-request stream)
    (make-error-response "not yet implemented")))

(define-handler get-date-time (stream)
  (multiple-value-bind (second minute hour day month year) (decode-universal-time (get-universal-time))
    (make-response 'date-time-response
                   nil
                   :date (format nil "~4,'0D~2,'0D~2,'0D" year month day)
                   :time (format nil "~4,'0D~2,'0D~2,'0D" hour minute second))))

(define-handler end-protocol (stream)
  (throw 'end-protocol nil))

