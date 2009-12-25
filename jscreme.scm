(define (display-to-string x)
  (call-with-output-string (lambda (port) (display x port))))

(load "compiler.scm")

(define (read-file name)
  (call-with-input-file name
    (lambda (port)
      (letrec ((recur (lambda ()
			(let ((expr (read port)))
			  (if (eof-object? expr)
			      '()
			      (cons expr (recur)))))))
	(recur)))))

(load "repl.scm")

(define (compile-toplevels exprs)
  (fold-left string-append "" (map (lambda (x) (toplevel-compile x read-file)) exprs)))

(call-with-output-file "tests.js"
  (lambda (port)
    (display (compile-toplevels '((load "primitives.scm")
				  (load "compiler.scm")
				  (load "reader.scm")
				  (load "tests.scm")))
	     port)))
