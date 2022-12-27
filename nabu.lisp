;; -*- Lisp -*-

(defpackage :nabu
  (:use :cl :alexandria))

(in-package :cserial-port)

;; monkey patch cserial-port library

(defmethod %open ((s posix-serial)
		  &key
		    name)
  (let* ((ratedef (%baud-rate s))
	 (fd (open name (logior o-rdwr o-noctty))))
    (when (= -1 fd)
      (error "~A open error!!" name))
    (setf (slot-value s 'fd) fd)
    (with-foreign-object (tty '(:struct termios))
      (unless (and
	       (zerop (tcgetattr fd tty))
	       (zerop (cfsetispeed tty ratedef))
	       (zerop (cfsetospeed tty ratedef)))
	(%close fd)
	(error "~A setspeed error!!" name))

      (with-foreign-slots ((lflag iflag cflag oflag cc) tty (:struct termios))
	(setf lflag (off lflag ICANON ECHO ECHONL IEXTEN ISIG))
	(setf iflag (off iflag BRKINT ICRNL INPCK ISTRIP IXON))
	(setf cflag (logior (off cflag PARENB CSTOPB CSIZE)
			    (%data-bits s)
			    (%parity s)
                            (%stop-bits s)
			    HUPCL CLOCAL))
	(setf oflag (off oflag OPOST))
	(setf (mem-aref cc 'cc-t VTIME) 0)
	(setf (mem-aref cc 'cc-t VMIN) 1))
      (unless (zerop (tcsetattr fd TCSANOW tty))
	(%close fd)
	(error "unable to setup serial port"))
      s)))

(defmethod %stop-bits ((s posix-serial) &optional stop-bits)
  (let ((val (or stop-bits (serial-stop-bits s))))
    (case val
      (1 0)
      (2 CSTOPB)
      (t (error "unsupported stop bits ~A" val)))))

(in-package :nabu)

;; CRC handling

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

;; Data types used on the wire

(binary-types:define-unsigned packet-length 2 :little-endian)
(binary-types:define-unsigned storage-size 2 :little-endian)

(binary-types:define-unsigned segment-number 3 :big-endian)
(binary-types:define-unsigned tier 4 :big-endian)
(binary-types:define-unsigned extended-packet-number 1 :little-endian)
(binary-types:define-unsigned offset 3 :big-endian)

(binary-types:define-binary-class packet-header ()
  ((segment-number :binary-type segment-number)
   (packet-number :binary-type binary-types:u8)
   (owner :binary-type binary-types:u8)
   (tier :binary-type tier)
   (mystery-byte-1 :binary-type binary-types:u8)
   (mystery-byte-2 :binary-type binary-types:u8)
   (flags :binary-type binary-types:u8)
   (extended-packet-number :binary-type extended-packet-number)
   (offset :binary-type offset)))

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
    (setf (ldb (byte 8 24) value) (read-byte stream))
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

(define-handler #x81 channel-confirm (stream)
  (send-ack stream))

(define-handler #x82 get-status (stream)
  (send-ack stream)
  (write-byte (logior (if *select-channel-p* #x80 #x00) #x1f) stream)
  (write-sequence #(#x10 #xe1) stream))

(define-handler #x83 set-status (stream)
  (send-ack stream)
  (write-byte #xe4 stream))

(define-handler #x8f whats-up (stream))

(define-handler #x05 more-ceremony (stream)
  (write-byte #xe4 stream))

(define-handler #x0f unknown-0f (stream)
  ; ignore for now
  )

(define-handler #x1e in-menu (stream)
  (write-byte #xe4 stream))

(defvar *time-packet* #(#x7F #xFF #xFF #x00 #x00 #x7F #xFF #xFF
                        #xFF #x7F #x80 #x30 #x00 #x00 #x00 #x00
                        #x02 #x02 #x02 #x54 #x01 #x01 #x00 #x00
                        #x00))

(defun send-confirm (stream)
  (write-byte #xe4 stream))

(defun send-time-packet (stream)
  (format t "; sending time packet~%")
  (send-packet-prolog stream)
  (write-sequence (add-crc *time-packet*) stream)
  (send-packet-epilog stream))

(defvar *segment-directory* *default-pathname-defaults*)
(defvar *segment-type* :nabu)

(defun make-segment-pathname (segment type)
  (merge-pathnames (make-pathname :name (format nil "~6,'0D" segment) :type type) *segment-directory*))

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

(defun write-escaped (packet stream)
  (loop for char across packet
        do (write-byte char stream)
        when (eql char +escape+)
          do (write-byte char stream)))

(defmethod file-position-at-packet ((type (eql :nabu)) file packet)
  (let ((offset (* packet +packet-size+)))
    (unless (file-position file offset)
      (format t "; could not seek to packet position ~A~%" offset)
      (throw 'main-loop nil))))

(defmethod make-packet ((type (eql :nabu))
                        segment-number
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
                                ,(ldb (byte 8 16) offset)
                                ,(ldb (byte 8 8) offset)
                                ,(ldb (byte 8 0) offset))
                             s)
             (write-sequence payload s))))

(defun send-segment-packet (stream segment-number packet-number &aux (type *segment-type*))
  (block nil
    (let ((pathname (make-segment-pathname segment-number (string-downcase type))))
      (with-open-file (file pathname :direction :input :if-does-not-exist nil :element-type '(unsigned-byte 8))
        (unless file
          (format t "; file ~A not found~%" pathname)
          (throw 'main-loop nil))
        (let ((offset (* packet-number +packet-size+)))
          (file-position-at-packet type file packet-number)
          (let* ((payload (make-array +packet-size+ :element-type '(unsigned-byte 8)))
                 (length (read-sequence payload file)))
            (send-packet-prolog stream)
            (write-escaped (make-packet type
                                        segment-number
                                        packet-number
                                        offset
                                        (>= (+ offset length) (file-length file))
                                        payload)
                            stream)
            (send-packet-epilog stream)))))))

(define-handler #x84 packet-request (stream)
  (send-ack stream)
  (finish-output stream)
  (let* ((request (read-long stream))
         (segment-number (ldb (byte 8 8) request))
         (packet (ldb (byte 8 0) request)))
    (send-confirm stream)
    (if (= request #x7fffff00)
        (send-time-packet stream)
        (send-segment-packet stream segment-number packet))))

(defvar *select-channel-p* nil)

(define-handler #x85 change-channel (stream)
  (send-ack stream)
  (finish-output stream)
  (let ((channel (read-short stream)))
    (format t "; channel: ~A~%" channel)
    (send-confirm stream)))

(defun make-message ()
  (make-string-input-stream (format nil "Current time: ~A~%" (get-universal-time))))

(define-handler #xa0 chat-started (stream)
  (with-open-stream (stream (flex:make-flexi-stream stream))
    (let ((server-message-stream (make-message))
          (client-message-stream (make-string-output-stream)))
      (loop
        (when-let ((c (read-char-no-hang stream)))
          (if (member c '(#\NewLine #\Linefeed #\Return))
              (let ((client-message (get-output-stream-string client-message-stream)))
                (format t "Client message: ~A~%" client-message)
                (setf client-message-stream (make-string-output-stream)
                      server-message-stream (make-string-input-stream (format nil "~%~A~%" client-message))))
              (princ c client-message-stream)))
        (if-let ((c (read-char server-message-stream nil)))
          (progn 
            (write-char c stream)
            (finish-output stream)
            (sleep .1))
          (setf server-message-stream (make-message)))))))

(define-handler #xa1 echo-line (stream)
  (let ((message-buffer (make-string-output-stream)))
    (loop
      (let ((c (read-byte stream)))
        (case c
          (#x83
           (format t "; NABU PC reset~%")
           (return))
          (#x0a
           (let ((line (get-output-stream-string message-buffer)))
             (format t "; Received [~A]~%" line)
             (loop for c across line
                   do (write-byte (char-code c) stream)
                   finally (write-byte #x0a stream)
                           (finish-output stream))))
          (otherwise
           (when (<= 32 c 126)
             (write-char (code-char c) message-buffer))))))))

;; Storage handling compatible to DJsure's NABU-LIB

(defvar *buffers* (make-array 256 :initial-element #()))

(define-handler #xa3 request-store-http-get (stream)
  (handler-case
      (let* ((index (read-byte stream))
             (url-length (read-byte stream))
             (url (make-string url-length)))
        (loop for i below url-length
              do (setf (aref url i) (code-char (read-byte stream))))
        (format t "; request URL ~A~%" url)
        (setf (aref *buffers* index) (flex:string-to-octets (format nil "Hello world, this is from ~A~%" url)))
        (write-byte 1 stream))
    (error (e)
      (format t "; failed to get URL: ~A" e)
      (write-byte 0 stream))))

(define-handler #xa4 request-store-get-size (stream)
  (let ((index (read-byte stream)))
    (binary-types:write-binary 'storage-size stream (length (aref *buffers* index)))))


(define-handler #xa5 request-store-get-data (stream)
  (let* ((index (read-byte stream))
         (start (binary-types:read-binary 'storage-size stream))
         (length (binary-types:read-binary 'storage-size stream))
         (end (+ start length))
         (buffer (aref *buffers* index)))
    #+slow-nabu
    (sleep .001)
    (format t "; index ~A total ~A start ~A end ~A~%" index (length buffer) start end)
    #+slow-nabu
    (loop for c across (subseq buffer start end)
          do (write-byte c stream)
             (finish-output stream)
             (sleep .001))
    #-slow-nabu
    (write-sequence buffer stream
                    :start start :end end)))

(defun handle-nabu (stream)
  (handler-case
      (loop
        (catch 'main-loop
          (handle-byte stream (read-byte stream))
          (finish-output stream)))
    (error (e)
      (format t "; caught error ~A~%" e))))

(defvar *server* nil)
(defvar *listen-socket*)

(defun run-server (port)
  (let* ((*listen-socket* (usocket:socket-listen "127.0.0.1" port)))
    (format t "; NABU listener on port ~A starting~%" port)
    (handler-case
         (loop
           (let ((connection (usocket:socket-accept *listen-socket* :element-type '(unsigned-byte 8))))
             (bt:make-thread (lambda ()
                               (let ((stream (usocket:socket-stream connection)))
                                 (format t "; connection established~%")
                                 (unwind-protect
                                      (handle-nabu stream)
                                   (format t "; closing connection~%")
                                   (usocket:socket-close connection)))))))
      (usocket:bad-file-descriptor-error (e)
        (declare (ignore e))
        (format t "; NABU listener on port ~A ending~%" port)))))

(defun stop-server ()
  (when *server*
    (bt:interrupt-thread *server* (lambda () (usocket:socket-close *listen-socket*)))
    (loop while (bt:thread-alive-p *server*)
          do (format t "; waiting for listener to shut down~%")
             (sleep 1))
    (setf *server* nil)))

(defun create-server (&optional (port 5816))
  (stop-server)
  (setf *server* (bt:make-thread (lambda () (run-server port)) :name (format nil "NABU Listener on port ~A" port))))

(defun handle-serial-port (port-name)
  (let ((serial (cserial-port:open-serial port-name
                  :baud-rate 115200
                  :data-bits 8
                  :stop-bits 2)))
    (with-open-stream (stream (sb-sys:make-fd-stream (cserial-port::serial-fd serial)
                                        :input t :output t
                                        :element-type '(unsigned-byte 8)))
      (handle-nabu stream))))

(defun pak-to-nabu (pathname &key (if-exists :error) (debug nil))
  (with-open-file (f pathname :element-type '(unsigned-byte 8))
    (with-open-file (nabu-file (merge-pathnames (make-pathname :type "nabu") pathname)
                               :direction :output :if-exists if-exists :element-type '(unsigned-byte 8))
      (format t "; Writing ~A~%" (pathname nabu-file))
      ;; file id bytes
      (handler-case
          (loop
            (let* ((raw-length (binary-types:read-binary 'packet-length f))
                   (packet (make-array raw-length :element-type '(unsigned-byte 8))))
              (assert (eql (read-sequence packet f) raw-length))
              (flex:with-input-from-sequence (packet-stream packet)
                (when debug
                  (format t "LENGTH: ~A~%" raw-length)
                  (with-slots (segment-number packet-number owner tier mystery-byte-1
                               mystery-byte-2 flags extended-packet-number offset)
                      (binary-types:read-binary 'packet-header packet-stream)
                    (format t "SEGMENT: ~A PACKET: ~A~%" segment-number packet-number)
                    (format t "OWNER: ~A TIER: ~8,'0X~%" owner tier)
                    (format t "FLAGS: ~8,'0B EXTENDED-PACKET-NUMBER: ~A OFFSET: ~A~%" flags extended-packet-number offset)))
                (let* ((payload-length (- raw-length 18))
                       (buf (make-array payload-length :element-type '(unsigned-byte 8))))
                  (format t "PAYLOAD-LENGTH ~A~%" payload-length)
                  (assert (= (read-sequence buf packet-stream) payload-length))
                  (write-sequence buf nabu-file)))))
        (end-of-file ())))))
