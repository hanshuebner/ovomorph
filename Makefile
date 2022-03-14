
all:
	buildapp --output ovomorph \
		--load $$HOME/quicklisp/setup.lisp \
		--eval '(ql:quickload :ovomorph)' \
		--entry ovomorph:main
