(define (make-toplevel-binding name value)
  (cons (jsify-symbol name) value))

(define *toplevel-bindings* '())

(define *toplevel-macros* '())

(define (add-toplevel-macro name func)
  (set! *toplevel-macros* (cons (cons name func) *toplevel-macros*)))

(define (add-toplevel-binding name var-name)
  (set! *toplevel-bindings* (cons (cons name var-name) *toplevel-bindings*)))

(define (macroexpand expr)
  (if (pair? expr)
      (if (eq? (car expr) 'quote)
	  expr
	  (let ((binding (assq (car expr) *toplevel-macros*))
		(rest (map (lambda (x) (macroexpand x)) (cdr expr))))
	    (if binding
		(macroexpand (apply (cdr binding) rest))
		(cons (macroexpand (car expr)) rest))))
      expr))

(define (toplevel-compile expr read-file)
  (if (and (pair? expr) (eq? (car expr) 'defmacro))
      (let ((name (cadr expr))
	    (args (caddr expr))
	    (value (car (cdddr expr))))
	(add-toplevel-macro name (eval (list 'lambda args
					     (macroexpand value))
				       (interaction-environment)))
	"")
      (let ((expr (macroexpand expr)))
	(cond ((and (pair? expr) (eq? (car expr) 'js-define))
	       (let* ((name (cadr expr))
		      (value (caddr expr))
		      (var-name (jsify-symbol name)))
		 (add-toplevel-binding name var-name)
		 (string-append "var " var-name "="
				(compile (macroexpand value) *toplevel-bindings*) ";\n")))
	      ((and (pair? expr) (eq? (car expr) 'load))
	       (let ((more-exprs (read-file (cadr expr))))
		 (fold-left string-append "" (map (lambda (x) (toplevel-compile x read-file)) more-exprs))))
	      (else
	       (string-append (compile expr *toplevel-bindings*) ";\n"))))))
