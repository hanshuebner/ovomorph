# ovomorph - I/O-server for the Apple2-IO-RPi board written in Common Lisp

This is an alternative implementation of an I/O server for the Apple
II utilizing the
[Apple2-IO-RPi](https://github.com/tjboldt/Apple2-IO-RPi) board by
Terence J. Boldt.  It is developed using [SBCL](http://www.sbcl.org/)
but uses compatibility libraries for portability to other Common Lisp
implementations.

## Installation

Presently, ovomorph needs to be installed from source.  Some system
dependencies are required:

    sudo apt install -y gpiod libgpiod2 sbcl

An installation script is provided:

    sh install.sh
    
The `install.sh` script will install the
[quicklisp](https://www.quicklisp.org/beta/) library manager for
Common Lisp and set things up so that ovomorph can be loaded.  It will
also install [buildapp](https://www.xach.com/lisp/buildapp/) which is
a tool to create standalone executables from Lisp programs.

## Status

Communication with the Apple II by the way of
[emulation](https://github.com/hanshuebner/jace) works.  At this
point, only disk I/O is implemented and largely untested.

