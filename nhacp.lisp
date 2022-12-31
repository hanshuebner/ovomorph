;; -*- Lisp -*-

(defpackage :nhacp
  (:use :cl :alexandria :binary-types)
  (:export conversation))

(in-package :nhacp)

(defconstant +max-payload-size+ 32768)

(defun write-bytes (string stream)
  (write-sequence (flex:string-to-octets string) stream))

(defun format-bytes (format &rest args)
  (flex:string-to-octets (apply #'format nil format args)))

(defun read-bytes (length stream)
  (let ((bytes (make-array length :element-type '(unsigned-byte 8))))
    (read-sequence bytes stream)
    (flex:octets-to-string bytes)))

(defun make-response (type payload &rest args)
  (let ((response (apply #'make-instance type args)))
    (flex:with-output-to-sequence (s)
      (write-byte (slot-value response 'type-tag) s)
      (binary-types:write-binary-record response s)
      (when payload
        (write-sequence payload s)))))

(defun make-error-response (format &rest args)
  (let ((message (apply #'format nil format args)))
    (format t "; Returning error to NABU: ~A~%" message)
    (make-response 'error-response
                   (flex:string-to-octets message)
                   :message-length (length message))))

(defgeneric handle-request (type-tag stream)
  (:method (type-tag stream)
    (unless (zerop (logand #x80 type-tag))
      (format t "; received command byte 0x~2,'0X with high-order bit set, aborting protocol~%" type-tag)
      (throw 'end-protocol nil))
    (make-error-response "Unknown NAHCP request tag 0x~2,'0X" type-tag))
  (:method :before (type-tag stream)
    (format t "; NHACP Request: ~A~%"
            (gethash type-tag *type-tag-to-name* (format nil "0x~2,'0X" type-tag)))))

(defmacro define-handler (name (stream) &body body)
  (with-gensyms (type-tag)
    `(defmethod handle-request ((,type-tag (eql ,(find-symbol (format nil "+~A-~A+" '#:NHACP-REQ name)))) ,stream)
       ,@body)))

(binary-types:define-unsigned frame-length 2 :little-endian)

(defun read-payload (stream length)
  (when (> length +max-payload-size+)
    (format t "; received overlong frame size 0x~4,'0X - NABU rebooting?  Ending NHACP.~%" length)
    (throw 'end-protocol nil))
  (let ((payload (make-array length :element-type '(unsigned-byte 8))))
    (read-sequence payload stream)
    payload))

(defun write-response (response stream)
  (format t "; Response: ~A (~A bytes)~%" (gethash (aref response 0) *type-tag-to-name* (aref response 0)) (length response))
  (binary-types:write-binary 'frame-length stream (length response))
  (write-sequence response stream)
  (finish-output stream))

(defun ensure-response-array (response)
  (if (typep response '(array))
      response
      (make-error-response "Handler returned no valid response array")))

(defun handle-stream (stream)
  (let* ((frame-length (binary-types:read-binary 'frame-length stream))
         (type-tag (read-byte stream))
         (payload (read-payload stream (1- frame-length))))
    (format t "Request: frame-length ~D type-tag ~2,'0X payload ~A~%" frame-length type-tag payload)
    (let ((response (ensure-response-array (flex:with-input-from-sequence (stream payload)
                                             (handle-request type-tag stream)))))
      (format t "Response: ~A~%" response)
      (write-response response stream)
      (finish-output stream))))

(defun send-adapter-id (stream)
  (let ((adapter-id (format-bytes "OVOMORPH running on ~A" (osicat-posix:gethostname))))
    (write-response (make-response 'nhacp-started-response
                                   adapter-id
                                   :version 1
                                   :adapter-id-length (length adapter-id))
                    stream)))

(defun conversation (stream)
  (format t "; starting NHACP protocol handler~%")
  (catch 'end-protocol
    (send-adapter-id stream)
    (loop
      (handle-stream stream))))
