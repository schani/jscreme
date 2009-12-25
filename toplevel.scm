(define (make-toplevel-binding name value)
  (cons (jsify-symbol name) value))

(define *toplevel-bindings* '())

(define *toplevel-macros* '())

(define (add-toplevel-macro name func)
  (set! *toplevel-macros* (cons (cons name func) *toplevel-macros*)))

(define (add-toplevel-binding name var-name)
  (set! *toplevel-bindings* (cons (cons name var-name) *toplevel-bindings*)))

(define (macroexpand expr)
  (letrec ((subexpand (lambda (l)
			(if (pair? l)
			    (cons (macroexpand (car l))
				  (subexpand (cdr l)))
			    l))))
    (if (pair? expr)
	(if (eq? (car expr) 'quote)
	    expr
	    (let ((binding (assq (car expr) *toplevel-macros*))
		  (rest (subexpand (cdr expr))))
	      (if binding
		  (macroexpand (apply (cdr binding) rest))
		  (cons (macroexpand (car expr)) rest))))
	expr)))
