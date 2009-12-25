all : jscreme.js

jscreme.js : compiler.scm eval.scm jscreme.scm primitives.scm reader.scm repl.scm \
		tests.scm toplevel-compile.scm toplevel.scm utils.scm
	guile -c '(begin (load "utils.scm") (load "jscreme.scm"))'

clean :
	rm jscreme.js
