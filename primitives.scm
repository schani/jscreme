(defmacro define (name value)
  (if (pair? name)
      (let ((name (car name))
	    (args (cdr name)))
	(list 'js-define name (list 'lambda args value)))
      (list 'js-define name value)))

(defmacro let (bindings . bodies)
  (apply list (apply list 'lambda (map car bindings) bodies) (map cadr bindings)))

(defmacro begin bodies
  (list (apply list 'lambda '() bodies)))

(defmacro cond cases
  (letrec ((recur (lambda (cases)
		    (if (null? cases)
			#f
			(if (eq? (caar cases) 'else)
			    (cadar cases)
			    (list 'if (caar cases) (cadar cases) (recur (cdr cases))))))))
    (recur cases)))

(defmacro and args
  (letrec ((recur (lambda (args)
		    (if (null? (cdr args))
			(car args)
			(list 'if (car args) (recur (cdr args)) #f)))))
    (if (null? args)
	#t
	(recur args))))

(defmacro or args
  (letrec ((recur (lambda (args)
		    (if (null? (cdr args))
			(car args)
			(list 'if (car args) #t (recur (cdr args)))))))
    (if (null? args)
	#f
	(recur args))))

(defmacro case (val . cases)
  (let ((sym '-*-val-*-))
    (letrec ((recur (lambda (args)
		      (if (null? args)
			  #f
			  (let ((cases (caar args))
				(value (cadar args)))
			    (if (eq? cases 'else)
				value
				(list 'if (cons 'or (map (lambda (x) (list 'eq? sym x)) cases))
				      value
				      (recur (cdr args)))))))))
      (list 'let (list (list sym val))
	    (recur cases)))))

(define (not x)
  (if x #f #t))

(define (eq? a b)
  (js-op a "==" b))

(define (null? x)
  (eq? x '()))

(define (number? x)
  (and (not (null? x))
       (js-op (.. x constructor) "==" (js-quote "Number"))))

(define (= a b)
  (js-op a "==" b))

(define (< a b)
  (js-op a "<" b))

(define (number->string x)
  ((.. x toString)))

(define (pair? x)
  (and (not (null? x))
       (js-op (.. x constructor) "==" (js-quote "Object"))
       (js-op "car" "in" x)))

(define (car x)
  (.. x car))

(define (cdr x)
  (.. x cdr))

(define (cons a b)
  (js-object (car a) (cdr b)))

(define (list & args)
  args)

(define (vector-ref v i)
  ((js-quote "function(v,i){return v[i];}") v i))

(define (vector-set! v i obj)
  ((js-quote "function(v,i,obj){v[i]=obj;}") v i obj))

(define (vector-length v)
  (.. v length))

(define *interned* (js-object))

(define (intern str)
  (if (js-op str "in" *interned*)
      (vector-ref *interned* str)
      (let ((sym (js-object (symbol str))))
	(vector-set! *interned* str sym)
	sym)))

(define (symbol? x)
  (and (not (null? x))
       (js-op (.. x constructor) "==" (js-quote "Object"))
       (js-op "symbol" "in" x)))

(define (symbol->string x)
  (.. x symbol))

(load "utils.scm")

(define (+ & args)
  (fold-left (lambda (a b) (js-op a "+" b)) 0 args))

(define (- x & rest)
  (if (null? rest)
      (js-op 0 "-" x)
      (js-op x "-" (apply + rest))))

(define (vector? x)
  (and (not (null? x))
       (js-op (.. x constructor) "==" (js-quote "Array"))))

(define (listify-vector vec start)
  (letrec ((recur (lambda (i l)
		    (if (< i start)
			l
			(recur (js-op i "-" 1) (cons (vector-ref vec i) l))))))
    (recur (js-op (vector-length vec) "-" 1) '())))

(define (vector->list vec)
  (listify-vector vec 0))

(define (list->vector l)
  (let ((vec (js-quote "[]")))
    (letrec ((recur (lambda (l)
		      (if (null? l)
			  vec
			  (begin
			    ((.. vec push) (car l))
			    (recur (cdr l)))))))
      (recur l))))

(define (vector & args)
  (list->vector args))

(define (error & args)
  ((js-quote "function (x){throw x;}") (list->vector args)))

(defmacro assert (x)
  (let ((fail-message (string-append "assertion " (display-to-string x) " failed")))
    (list 'if x
	  #t
	  (list 'error fail-message))))

(define (string? x)
  (and (not (null? x))
       (js-op (.. x constructor) "==" (js-quote "String"))))

(define (string-length s)
  (.. s length))

(define (string-ref s i)
  ((js-quote "function(s,i){return s[i];}") s i))

(define (string-append & args)
  (fold-left (lambda (a b) (js-op a "+" b)) "" args))

(define string string-append)

(define (map f l)
  (fold-right (lambda (x xs) (cons (f x) xs)) l '()))

(define (length l)
  (fold-left (lambda (count x) (+ count 1)) 0 l))

(define (equal? a b)
  (cond ((pair? a)
	 (and (pair? b)
	      (equal? (car a) (car b))
	      (equal? (cdr a) (cdr b))))
	((vector? a)
	 (and (vector? b)
	      (let ((len (vector-length a)))
		(and (= len (vector-length b))
		     (letrec ((recur (lambda (i)
				       (or (= i len)
					   (and (equal? (vector-ref a i) (vector-ref b i))
						(recur (+ i 1)))))))
		       (recur 0))))))
	(else
	 (eq? a b))))

(define (display-to-string x)
  (cond ((null? x)
	 "()")
	((number? x)
	 (number->string x))
	((string? x)
	 x)
	((symbol? x)
	 (symbol->string x))
	((pair? x)
	 (letrec ((recur (lambda (x so-far)
			   (if (pair? x)
			       (if (null? (cdr x))
				   (string-append so-far (display-to-string (car x)) ")")
				   (recur (cdr x) (string-append so-far (display-to-string (car x)) " ")))
			       (string-append so-far ". " (display-to-string x) ")")))))
	   (recur x "(")))
	((vector? x)
	 (let ((last (- (vector-length x) 1)))
	   (letrec ((recur (lambda (i so-far)
			     (if (< i last)
				 (recur (+ i 1) (string-append so-far (display-to-string (vector-ref x i)) " "))
				 so-far))))
	     (string-append (recur 0 "[")
			    (if (= last -1) "" (display-to-string (vector-ref x last)))
			    "]"))))
	(else
	 (error 'cannot-display))))
