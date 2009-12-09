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

(define (compile-toplevels exprs)
  (letrec ((macroexpand (lambda (expr macros)
			  (if (pair? expr)
			      (if (eq? (car expr) 'quote)
				  expr
				  (let ((binding (assq (car expr) macros))
					(rest (map (lambda (x) (macroexpand x macros)) (cdr expr))))
				    (if binding
					(macroexpand (apply (cdr binding) rest) macros)
					(cons (macroexpand (car expr) macros) rest))))
			      expr)))
	   (recur (lambda (so-far exprs env macros)
		    (cond ((null? exprs)
			   so-far)
			  ((and (pair? (car exprs)) (eq? (caar exprs) 'defmacro))
			   (let ((name (cadar exprs))
				 (args (caddar exprs))
				 (value (car (cdddar exprs))))
			     (recur so-far
				    (cdr exprs) env
				    (cons (cons name (eval (list 'lambda args
								 (macroexpand value macros))
							   (interaction-environment)))
					  macros))))
			  (else
			   (let ((expr (macroexpand (car exprs) macros)))
			     (cond ((and (pair? expr) (eq? (car expr) 'js-define))
				    (let* ((name (cadr expr))
					   (value (caddr expr))
					   (var-name (jsify-symbol name))
					   (new-env (cons (cons name var-name) env)))
				      (recur (string-append so-far "var " var-name "="
							    (compile (macroexpand value macros) new-env) ";\n")
					     (cdr exprs) new-env macros)))
				   ((and (pair? expr) (eq? (car expr) 'load))
				    (let ((more-exprs (read-file (cadr expr))))
				      (recur so-far (append more-exprs (cdr exprs)) env macros)))
				   (else
				    (recur (string-append so-far (compile expr env) ";\n")
					   (cdr exprs) env macros)))))))))
    (recur "" exprs '() '())))

(call-with-output-file "tests.js"
  (lambda (port)
    (display (compile-toplevels '((load "primitives.scm")
				  (load "compiler.scm")
				  (load "reader.scm")
				  (load "tests.scm")))
	     port)))
