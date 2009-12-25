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
  (letrec ((recur (lambda (so-far exprs env)
		    (cond ((null? exprs)
			   so-far)
			  ((and (pair? (car exprs)) (eq? (caar exprs) 'defmacro))
			   (let ((name (cadar exprs))
				 (args (caddar exprs))
				 (value (car (cdddar exprs))))
			     (add-macro name (eval (list 'lambda args
							 (macroexpand value))
						   (interaction-environment)))
			     (recur so-far (cdr exprs) env)))
			  (else
			   (let ((expr (macroexpand (car exprs))))
			     (cond ((and (pair? expr) (eq? (car expr) 'js-define))
				    (let* ((name (cadr expr))
					   (value (caddr expr))
					   (var-name (jsify-symbol name))
					   (new-env (cons (cons name var-name) env)))
				      (recur (string-append so-far "var " var-name "="
							    (compile (macroexpand value) new-env) ";\n")
					     (cdr exprs) new-env)))
				   ((and (pair? expr) (eq? (car expr) 'load))
				    (let ((more-exprs (read-file (cadr expr))))
				      (recur so-far (append more-exprs (cdr exprs)) env)))
				   (else
				    (recur (string-append so-far (compile expr env) ";\n")
					   (cdr exprs) env)))))))))
    (recur "" exprs '())))

(call-with-output-file "tests.js"
  (lambda (port)
    (display (compile-toplevels '((load "primitives.scm")
				  (load "compiler.scm")
				  (load "reader.scm")
				  (load "tests.scm")))
	     port)))
