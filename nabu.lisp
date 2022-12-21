;; -*- Lisp -*-

(defpackage :nabu
  (:use :cl :alexandria))

(in-package :nabu)

(defvar *crc-table*
  #(#x0000 #x1021 #x2042 #x3063 #x4084 #x50a5 #x60c6 #x70e7 #x8108 #x9129 #xa14a #xb16b
    #xc18c #xd1ad #xe1ce #xf1ef #x1231 #x0210 #x3273 #x2252 #x52b5 #x4294 #x72f7 #x62d6
    #x9339 #x8318 #xb37b #xa35a #xd3bd #xc39c #xf3ff #xe3de #x2462 #x3443 #x0420 #x1401
    #x64e6 #x74c7 #x44a4 #x5485 #xa56a #xb54b #x8528 #x9509 #xe5ee #xf5cf #xc5ac #xd58d
    #x3653 #x2672 #x1611 #x0630 #x76d7 #x66f6 #x5695 #x46b4 #xb75b #xa77a #x9719 #x8738
    #xf7df #xe7fe #xd79d #xc7bc #x48c4 #x58e5 #x6886 #x78a7 #x0840 #x1861 #x2802 #x3823
    #xc9cc #xd9ed #xe98e #xf9af #x8948 #x9969 #xa90a #xb92b #x5af5 #x4ad4 #x7ab7 #x6a96
    #x1a71 #x0a50 #x3a33 #x2a12 #xdbfd #xcbdc #xfbbf #xeb9e #x9b79 #x8b58 #xbb3b #xab1a
    #x6ca6 #x7c87 #x4ce4 #x5cc5 #x2c22 #x3c03 #x0c60 #x1c41 #xedae #xfd8f #xcdec #xddcd
    #xad2a #xbd0b #x8d68 #x9d49 #x7e97 #x6eb6 #x5ed5 #x4ef4 #x3e13 #x2e32 #x1e51 #x0e70
    #xff9f #xefbe #xdfdd #xcffc #xbf1b #xaf3a #x9f59 #x8f78 #x9188 #x81a9 #xb1ca #xa1eb
    #xd10c #xc12d #xf14e #xe16f #x1080 #x00a1 #x30c2 #x20e3 #x5004 #x4025 #x7046 #x6067
    #x83b9 #x9398 #xa3fb #xb3da #xc33d #xd31c #xe37f #xf35e #x02b1 #x1290 #x22f3 #x32d2
    #x4235 #x5214 #x6277 #x7256 #xb5ea #xa5cb #x95a8 #x8589 #xf56e #xe54f #xd52c #xc50d
    #x34e2 #x24c3 #x14a0 #x0481 #x7466 #x6447 #x5424 #x4405 #xa7db #xb7fa #x8799 #x97b8
    #xe75f #xf77e #xc71d #xd73c #x26d3 #x36f2 #x0691 #x16b0 #x6657 #x7676 #x4615 #x5634
    #xd94c #xc96d #xf90e #xe92f #x99c8 #x89e9 #xb98a #xa9ab #x5844 #x4865 #x7806 #x6827
    #x18c0 #x08e1 #x3882 #x28a3 #xcb7d #xdb5c #xeb3f #xfb1e #x8bf9 #x9bd8 #xabbb #xbb9a
    #x4a75 #x5a54 #x6a37 #x7a16 #x0af1 #x1ad0 #x2ab3 #x3a92 #xfd2e #xed0f #xdd6c #xcd4d
    #xbdaa #xad8b #x9de8 #x8dc9 #x7c26 #x6c07 #x5c64 #x4c45 #x3ca2 #x2c83 #x1ce0 #x0cc1
    #xef1f #xff3e #xcf5d #xdf7c #xaf9b #xbfba #x8fd9 #x9ff8 #x6e17 #x7e36 #x4e55 #x5e74
    #x2e93 #x3eb2 #x0ed1 #x1ef0))

(defun crc16 (vector)
  (loop with crc = #xffff
        for byte across vector
        do (let ((index (logxor (ldb (byte 8 8) crc) byte)))
             (setf crc (ash crc 8)
                   crc (logand (logxor crc (aref *crc-table* index)) #xffff)))
        finally (return crc)))

(defconstant +escape+ #x10)

(defun add-crc (packet)
  (let ((crc (crc16 packet))
        (packet-with-crc (copy-array packet :adjustable t :fill-pointer (length packet))))
    (vector-push-extend (logxor (ldb (byte 8 8) crc) #xff) packet-with-crc)
    (vector-push-extend (logxor (ldb (byte 8 0) crc) #xff) packet-with-crc)
    packet-with-crc))

(defun read-short (stream)
  (let ((value (read-byte stream)))
    (setf (ldb (byte 8 8) value) (read-byte stream))
    value))

(defun read-long (stream)
  (let ((value (read-byte stream)))
    (setf (ldb (byte 8 8) value) (read-byte stream))
    (setf (ldb (byte 8 16) value) (read-byte stream))
    (setf (ldb (byte 8 16) value) (read-byte stream))
    value))

(defgeneric handle-byte (stream byte)
  (:method (stream byte)
    (declare (ignore stream))
    (format t "; Received 0x~2,'0X~%" byte)))

(defun send-ack (stream)
  (write-sequence #(#x10 #x06) stream))

(defmacro define-handler (byte name (stream) &body body)
  (with-gensyms (byte*)
    `(defmethod handle-byte (,stream (,byte* (eql ,byte)))
       (format t "; ~A~%" ',name)
       ,@body)))

(define-handler #x83 nabu-initialize (stream)
  (send-ack stream)
  (write-byte #xe4 stream))

(define-handler #x82 get-ready (stream)
  (send-ack stream))

(define-handler #x81 channel-confirm (stream)
  (send-ack stream))

(define-handler #x8f whats-up (stream))

(define-handler #x05 more-ceremony (stream)
  (write-byte #xe4 stream))

(defvar *time-packet* #(#x7F #xFF #xFF #x00 #x00 #x7F #xFF #xFF
                        #xFF #x7F #x80 #x30 #x00 #x00 #x00 #x00
                        #x02 #x02 #x02 #x54 #x01 #x01 #x00 #x00
                        #x00))

(defun send-confirm (stream)
  (write-byte #xe4 stream))

(defun send-time-packet (stream)
  (send-packet-prolog stream)
  (write-sequence (add-crc *time-packet*) stream)
  (send-packet-epilog stream))

(defun make-segment-pathname (segment)
  (merge-pathnames (make-pathname :name (format nil "~6,'0D" segment) :type "nabu")))

(defun expect (stream expected-byte)
  (finish-output stream)
  (let ((byte (read-byte stream)))
    (or (= byte expected-byte)
        (progn
          (format t "; expected 0x~2,'0X, received 0x~2,'0X~%" expected-byte byte)
          (throw 'main-loop nil)))))

(defun expect-confirmation (stream)
  (and (expect stream +escape+)
       (expect stream #x06)))

(defun send-packet-prolog (stream)
  (write-byte #x91 stream)
  (expect-confirmation stream))

(defun send-packet-epilog (stream)
  (write-sequence #(#x10 #xe1) stream))

(defconstant +packet-size+ 991)

(defun make-packet (segment-number
                    packet-number
                    offset
                    last-packet-p
                    payload)
  (add-crc (flex:with-output-to-sequence (s)
             (write-sequence `#(,(ldb (byte 8 16) segment-number)
                                ,(ldb (byte 8 8) segment-number)
                                ,(ldb (byte 8 0) segment-number)
                                ,(ldb (byte 8 0) packet-number)
                                #x01               ; owner
                                #x7f #xff #xff #xff ; tier
                                #x7f #x80           ; mystery bytes
                                ,(logior
                                  #x20
                                  (if last-packet-p #x10 #x00)
                                  (if (zerop packet-number) #x41 #x00))
                                ,(ldb (byte 8 0) packet-number)
                                ,(ldb (byte 8 8) packet-number)
                                ,(ldb (byte 8 8) offset)
                                ,(ldb (byte 8 0) offset))
                             s)
             (write-sequence payload s))))

(defun write-escaped (packet stream)
  (loop for char across packet
        do (write-byte char stream)
        when (eql char +escape+)
          do (write-byte char stream)))

(defun send-segment-packet (stream segment packet)
  (block nil
    (let ((pathname (make-segment-pathname segment)))
      (with-open-file (file pathname :direction :input :if-does-not-exist nil :element-type '(unsigned-byte 8))
        (unless file
          (format t "; file ~A not found~%" pathname)
          (throw 'main-loop nil))
        (let ((offset (* packet +packet-size+)))
          (unless (file-position file offset)
            (format t "; could not seek to packet position ~A~%" (* packet +packet-size+))
            (throw 'main-loop nil))
          (let* ((payload (make-array +packet-size+ :element-type '(unsigned-byte 8)))
                 (length (read-sequence payload file)))
            (send-packet-prolog stream)
            (write-escaped (make-packet segment
                                        packet
                                        offset
                                        (>= (+ offset length) (file-length file))
                                        payload)
                            stream)
            (send-packet-epilog stream)))))))

(define-handler #x84 packet-request (stream)
  (send-ack stream)
  (finish-output stream)
  (let* ((request (read-long stream))
         (segment (ldb (byte 8 8) request))
         (packet (ldb (byte 8 0) request)))
    (send-confirm stream)
    (if (= request #x7fffff00)
        (send-time-packet stream)
        (send-segment-packet stream segment packet))))

(define-handler #x01 channel-status (stream)
  #+ (or)
  (write-sequence #(#x9f #x10 #xe1) stream)
  (write-sequence #(#x1f #x10 #xe1) stream))

(define-handler #x85 change-channel (stream)
  (write-sequence #(#x10 #x06) stream)
  (finish-output stream)
  (let ((channel (read-short stream)))
    (format t "; channel: ~A~%" channel)
    (send-confirm stream)))

(define-handler #xa0 chat-started (stream)
  (with-open-stream (stream (flex:make-flexi-stream stream))
    (loop for c across "Hello World"
          do (write-char c stream)
             (finish-output stream)
             (sleep 0.03))))

(defun handle-connection (connection)
  (let ((stream (usocket:socket-stream connection)))
    (format t "; connection established~%")
    (unwind-protect
         (handler-case
             (loop
               (catch 'main-loop
                 (handle-byte stream (read-byte stream))
                 (finish-output stream)))
           (error (e)
             (format t "; caught error ~A~%" e)))
      (format t "; closing connection~%")
      (usocket:socket-close connection))))

(defun create-server (&optional (port 5816))
  (let* ((socket (usocket:socket-listen "127.0.0.1" port)))
    (unwind-protect
         (loop
           (let ((connection (usocket:socket-accept socket :element-type '(unsigned-byte 8))))
             (handle-connection connection)))
      (format t "; closing listening socket~%")
      (usocket:socket-close socket))))
