# ovomorph - I/O-server for the Apple2-IO-RPi board written in Common Lisp

This is an alternative implementation of an I/O server for the Apple
II utilizing the
[Apple2-IO-RPi](https://github.com/tjboldt/Apple2-IO-RPi) board by
Terence J. Boldt.  It is developed using [SBCL](http://www.sbcl.org/)
but uses compatibility libraries for portability to other Common Lisp
implementations.

## Status

Communication with the Apple II by the way of
[emulation](https://github.com/hanshuebner/jace) works.  At this
point, only disk I/O is implemented and largely untested.

