(define (make-toplevel-binding name value)
  (cons (jsify-symbol name) value))

(define *toplevel-bindings* '())

(define *toplevel-macros* '())

(define (add-macro name func)
  (set! *toplevel-macros* (cons (cons name func) *toplevel-macros*)))

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
