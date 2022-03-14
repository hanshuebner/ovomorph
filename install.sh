#!/bin/sh

set -e

QUICKLISP_BOOTSTRAP=/tmp/quicklisp.lisp
QUICKLISP_DIR=$HOME/quicklisp
QUICKLISP_SETUP=$QUICKLISP_DIR/setup.lisp
SOURCE=$(pwd)
MYNAME=$(echo $SOURCE | sed -e 's/.*\///')

if [ -f $QUICKLISP_SETUP ]
then
    echo "quicklisp is already set up"
else
    cat <<EOF
Setting up quicklisp

This will set up a quicklisp folder in your home directory and populate it.

See https://quicklisp.org/ for details.

EOF
    trap "rm $QUICKLISP_BOOTSTRAP" 0
    curl -Lo $QUICKLISP_BOOTSTRAP https://beta.quicklisp.org/quicklisp.lisp
    sbcl --no-userinit \
         --disable-debugger \
         --load /tmp/quicklisp.lisp \
         --eval '(quicklisp-quickstart:install)' \
         --eval '(ql:add-to-init-file)' \
         --eval '(ql:quickload :buildapp)' \
         --eval '(quit)'
fi

cd $QUICKLISP_DIR/dists/quicklisp/software/buildapp* && sudo make install

ln -sf $SOURCE $QUICKLISP_DIR/local-projects/$MYNAME
if ! [ -e $QUICKLISP_DIR/local-projects/cl-gpiod ]
then
    git clone https://github.com/hanshuebner/cl-gpiod $QUICKLISP_DIR/local-projects/cl-gpiod
fi

