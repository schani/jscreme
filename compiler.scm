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
						    ((#\space) "_space_")
						    (else (string c))))))
			  so-far))))
      (string-append "_" (recur 0 "") "_"))))

(define (quote-string str)
  (let ((len (string-length str)))
    (letrec ((recurse (lambda (i so-far)
			(if (< i len)
			    (let ((c (string-ref str i)))
			      (recurse (+ i 1)
				       (string-append so-far
						      (case c
							((#\") "\\\"")
							((#\\) "\\\\")
							((#\newline) "\\n")
							(else (string c))))))
			    so-far))))
      (string-append (recurse 0 "\"") "\""))))

;; () -> null
;; 123 -> 123
;; #\a -> "a"
;; "abc" -> "abc"
;; #t -> true
;; #f -> false
;; abc -> (string->symbol ("abc"))
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
	 (string-append "(" (jsify-symbol 'string->symbol) "(" (quote-string (symbol->string expr)) "))"))
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
    (cond ((or (number? expr) (char? expr) (string? expr) (boolean? expr))
	   (quote-to-string expr))
	  ((symbol? expr)
	   (let ((entry (assoc expr env)))
	     (if entry
		 (cdr entry)
		 (error 'symbol-unbound expr))))
	  ((pair? expr)
	   (case (car expr)
	     ((quote)
	      (quote-to-string (cadr expr)))
	     ((..)
	      (string-append "(" (compile (cadr expr) env) "." (symbol->string (caddr expr)) ")"))
	     ((js-object)
	      (string-append "{"
			     (commatize (map (lambda (x)
					       (string-append (symbol->string (car x))
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
		     (arglist (map (lambda (a) (cons a (jsify-symbol (gensym)))) (car deconstructed-arglist)))
		     (rest-arg (cadr deconstructed-arglist))
		     (rest-arg-name (jsify-symbol (gensym)))
		     (body-list (cddr expr))
		     (new-env (if rest-arg
				  (cons (cons rest-arg rest-arg-name) (append arglist env))
				  (append arglist env))))
		(string-append "function("
			       (commatize (map cdr arglist))
			       "){"
			       (if rest-arg
				   (string-append "var "
						  rest-arg-name
						  "="
						  (jsify-symbol 'listify-vector)
						  "(arguments,"
						  (number->string (length arglist))
						  ");")
				   "")
			       (compile-body-list body-list new-env)
			       "}")))
	     ((letrec)
	      (let* ((bindings (cadr expr))
		     (names (map (lambda (b) (cons (car b) (jsify-symbol (gensym)))) bindings))
		     (body-list (cddr expr))
		     (new-env (append names env)))
		(string-append "(function(){"
			       (apply string-append (map (lambda (binding)
							   (string-append "var "
									  (cdr (assoc (car binding) names))
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
	     ((set!)
	      (let* ((name (cadr expr))
		     (value (caddr expr))
		     (entry (assoc name env)))
		(if entry
		    (string-append "(" (cdr entry) "=" (compile value env) ")")
		    (error 'symbol-unbound name))))
	     (else
	      (let ((function (car expr))
		    (args (cdr expr)))
		(string-append "(" (compile function env) "(" (commatize (map (lambda (e) (compile e env)) args)) "))")))))
	  (else
	   (error 'cannot-compile expr)))))
