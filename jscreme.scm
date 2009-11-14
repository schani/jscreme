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
					(apply (cdr binding) rest)
					(cons (macroexpand (car expr) macros) rest))))
			      expr)))
	   (recur (lambda (exprs env macros)
		    (cond ((null? exprs)
			   "")
			  ((and (pair? (car exprs)) (eq? (caar exprs) 'defmacro))
			   (let ((name (cadar exprs))
				 (args (caddar exprs))
				 (value (car (cdddar exprs))))
			     (recur (cdr exprs) env
				    (cons (cons name (eval (list 'lambda args
								 (macroexpand value macros))
							   (interaction-environment)))
					  macros))))
			  (else
			   (let ((expr (macroexpand (car exprs) macros)))
			     (cond ((and (pair? expr) (eq? (car expr) 'js-define))
				    (let* ((name (cadr expr))
					   (value (caddr expr))
					   (new-env (cons name env)))
				      (string-append "var " (jsify-symbol name) "="
						     (compile (macroexpand value macros) new-env) ";\n"
						     (recur (cdr exprs) new-env macros))))
				   ((and (pair? expr) (eq? (car expr) 'load))
				    (let ((more-exprs (read-file (cadr expr))))
				      (recur (append more-exprs (cdr exprs)) env macros)))
				   (else
				    (string-append (compile expr env) ";\n"
						   (recur (cdr exprs) env macros))))))))))
    (recur exprs '() '())))

(call-with-output-file "tests.js"
  (lambda (port)
    (display (compile-toplevels '((load "primitives.scm")
				  (load "compiler.scm")
				  (load "tests.scm")))
	     port)))
