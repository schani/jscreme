(define (jsify-symbol sym)
  (let* ((str (symbol->string sym))
	 (len (string-length str)))
    (letrec ((recur (lambda (i so-far)
		      (if (< i len)
			  (recur (+ i 1)
				 (string-append so-far
						(let ((c (string-ref str i)))
						  (case c
						    ((#\?) "_qm_")
						    ((#\=) "_eq_")
						    ((#\<) "_lt_")
						    ((#\>) "_gt_")
						    ((#\-) "___")
						    ((#\+) "_plus_")
						    ((#\*) "_star_")
						    ((#\!) "_bang_")
						    (else (string c))))))
			  so-far))))
      (recur 0 ""))))

(define (quote-string str)
  (string-append "\"" str "\""))

;; () -> null
;; 123 -> 123
;; #\a -> "a"
;; "abc" -> "abc"
;; #t -> true
;; #f -> false
;; abc -> (intern ("abc"))
;; (a . b) -> { car: a, cdr: b }

(define (quote-to-string expr)
  (cond ((null? expr)
	 "null")
	((number? expr)
	 (number->string expr))
	((char? expr)
	 (quote-string (string expr)))
	((string? expr)
	 (quote-string expr))
	((boolean? expr)
	 (if expr
	     "true"
	     "false"))
	((symbol? expr)
	 (string-append "(intern(" (quote-string (symbol->string expr)) "))"))
	((pair? expr)
	 (string-append "{car:" (quote-to-string (car expr)) ",cdr:" (quote-to-string (cdr expr)) "}"))
	(else
	 (error 'cannot-quote expr))))

(define (commatize l)
  (if (null? l)
      ""
      (reduce (lambda (a b)
		(string-append a "," b))
	      l)))

(define (deconstruct-arglist l)
  (cond ((null? l)
	 '(() #f))
	((eq? (car l) '&)
	 (list '() (cadr l)))
	(else
	 (let ((rl (deconstruct-arglist (cdr l))))
	   (list (cons (car l) (car rl)) (cadr rl))))))

;; 123 -> 123
;; "abc" -> "abc"
;; #t -> true
;; #f -> false
;; abc -> abc
;; (.. x a) -> (x.a)
;; (js-object (a x) (b y)) -> { a: x, b: y }
;; (js-op a "==" b) -> (a == b)
;; (js-quote "null") -> null
;; (apply f a b c) -> (f.apply (null, [a, b].concat (list____gt_vector(c))))
;; (lambda (a b) c) -> function (a,b) { return c; }
;; (lambda (a b & c) d) -> function (a,b) { var c = listify___vector (arguments, 2); return d; }
;; (letrec/let* ((a x) (b y)) c) -> (function () { var a = x; var b = y; return c; } ())
;; (if a b c) -> (a ? b : c)
;; (f a b) -> (f (a, b))

(define (compile expr env)
  (display "compiling ") (display expr) (newline)
  (letrec ((compile-body-list (lambda (l env)
				(cond ((null? l)
				       "")
				      ((null? (cdr l))
				       (string-append "return " (compile (car l) env) ";"))
				      (else
				       (string-append (compile (car l) env) ";\n"
						      (compile-body-list (cdr l) env)))))))
    (cond ((or (number? expr) (string? expr) (boolean? expr))
	   (quote-to-string expr))
	  ((symbol? expr)
	   (let ((str (jsify-symbol expr)))
	     (if (memq expr env)
		 str
		 (error 'symbol-unbound expr))))
	  ((pair? expr)
	   (case (car expr)
	     ((quote)
	      (quote-to-string (cadr expr)))
	     ((..)
	      (string-append "(" (compile (cadr expr) env) "." (jsify-symbol (caddr expr)) ")"))
	     ((js-object)
	      (string-append "{"
			     (commatize (map (lambda (x)
					       (string-append (jsify-symbol (car x))
							      ":"
							      (compile (cadr x) env)))
					     (cdr expr)))
			     "}"))
	     ((js-op)
	      (string-append "(" (compile (cadr expr) env) " " (caddr expr) " " (compile (cadddr expr) env) ")"))
	     ((js-quote)
	      (cadr expr))
	     ((apply)
	      (letrec ((compile-args (lambda (args)
				       (if (null? (cdr args))
					   (string-append "].concat(" (jsify-symbol 'list->vector) "(" (car args) ")")
					   (string-append (car args) "," (compile-args (cdr args)))))))
		(string-append "("
			       (compile (cadr expr) env)
			       ".apply(null,["
			       (compile-args (map (lambda (x) (compile x env)) (cddr expr)))
			       ")))")))
	     ((lambda)
	      (let* ((deconstructed-arglist (deconstruct-arglist (cadr expr)))
		     (arglist (car deconstructed-arglist))
		     (rest-arg (cadr deconstructed-arglist))
		     (body-list (cddr expr))
		     (new-env (if rest-arg
				  (cons rest-arg (append arglist env))
				  (append arglist env))))
		(string-append "function("
			       (commatize (map jsify-symbol arglist))
			       "){"
			       (if rest-arg
				   (string-append "var "
						  (jsify-symbol rest-arg)
						  "="
						  (jsify-symbol 'listify-vector)
						  "(arguments,"
						  (number->string (length arglist))
						  ");")
				   "")
			       (compile-body-list body-list new-env)
			       "}")))
	     ((letrec let*) ;FIXME: the way we compile here is wrong, both for letrec and let*
	      (let* ((bindings (cadr expr))
		     (names (map car bindings))
		     (body-list (cddr expr))
		     (new-env (append names env)))
		(string-append "(function(){"
			       (apply string-append (map (lambda (binding)
							   (string-append "var "
									  (jsify-symbol (car binding))
									  "="
									  (compile (cadr binding) new-env)
									  ";" ))
							 bindings))
			       (compile-body-list body-list new-env)
			       "}())")))
	     ((if)
	      (let ((condition (cadr expr))
		    (consequent (caddr expr))
		    (alternative (if (cdddr expr) (cadddr expr) #f)))
		(string-append "("
			       (compile condition env) "?" (compile consequent env) ":"
			       (if alternative
				   (compile alternative env)
				   "null")
			       ")")))
	     (else
	      (let ((function (car expr))
		    (args (cdr expr)))
		(string-append "(" (compile function env) "(" (commatize (map (lambda (e) (compile e env)) args)) "))")))))
	  (else
	   (error 'cannot-compile expr)))))

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
				  (load "tests.scm")))
	     port)))
