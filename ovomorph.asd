;;;; -*- Mode: Lisp -*-

(in-package :cl-user)

(defpackage :ovomorph.system
  (:use :cl :asdf))

(in-package :ovomorph.system)

(defsystem :ovomorph
  :name "ovomorph"
  :author "Hans Hübner <hans.huebner@gmail.com>"
  :version "0.0.1"
  :maintainer "Hans Hübner <hans.huebner@gmail.com>>"
  :licence "BSD"
  :description "Apple 2 I/O-Server for Apple2-RPi-IO card"
  :long-description ""

  :depends-on (:alexandria
               :usocket
               :usocket-server
               :lparallel
               :binary-types
               :cserial-port
               :drakma
               :str
               #+linux
               :cl-gpiod
               :flexi-streams)
  :components ((:file "a2-comm")
               #-linux
               (:file "udp")
               #+linux
               (:file "gpio")
               (:file "ovomorph")
               (:file "disk")
               (:file "time")
               (:file "shell")
               (:file "nhacp")
               (:file "nabu-comms")
               (:file "nabu")))
