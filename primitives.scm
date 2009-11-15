(defmacro begin bodies
  (list (apply list 'lambda '() bodies)))

(defmacro define (name . body-list)
  (if (pair? name)
      (let ((name (car name))
	    (args (cdr name)))
	(list 'js-define name (apply list 'lambda args body-list)))
      (list 'js-define name (apply list 'begin body-list))))

(defmacro let (bindings . bodies)
  (apply list (apply list 'lambda (map car bindings) bodies) (map cadr bindings)))

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
				(list 'if (cons 'or (map (lambda (x) (list 'eq? sym (list 'quote x))) cases))
				      value
				      (recur (cdr args)))))))))
      (list 'let (list (list sym val))
	    (recur cases)))))

(define (not x)
  (if x #f #t))

(define (eq? a b)
  (js-op a "===" b))

(define (null? x)
  (eq? x '()))

(define (number? x)
  (and (not (null? x))
       (js-op (.. x constructor) "==" (js-quote "Number"))))

(define (= a b)
  (js-op a "===" b))

(define (< a b)
  (js-op a "<" b))

(define (number->string x)
  ((.. x toString)))

(define (string->number x)
  ((js-quote "parseInt") x))

(define (pair? x)
  (and (not (null? x))
       (js-op (.. x constructor) "==" (js-quote "Object"))
       (js-op "car" "in" x)))

(define (boolean? x)
  (or (eq? x #t) (eq? x #f)))

(define (car x)
  (.. x car))

(define (cdr x)
  (.. x cdr))

(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))

(define (caaar x) (car (car (car x))))
(define (caadr x) (car (car (cdr x))))
(define (cadar x) (car (cdr (car x))))
(define (caddr x) (car (cdr (cdr x))))
(define (cdaar x) (cdr (car (car x))))
(define (cdadr x) (cdr (car (cdr x))))
(define (cddar x) (cdr (cdr (car x))))
(define (cdddr x) (cdr (cdr (cdr x))))

(define (caaaar x) (car (car (car (car x)))))
(define (caaadr x) (car (car (car (cdr x)))))
(define (caadar x) (car (car (cdr (car x)))))
(define (caaddr x) (car (car (cdr (cdr x)))))
(define (cadaar x) (car (cdr (car (car x)))))
(define (cadadr x) (car (cdr (car (cdr x)))))
(define (caddar x) (car (cdr (cdr (car x)))))
(define (cadddr x) (car (cdr (cdr (cdr x)))))
(define (cdaaar x) (cdr (car (car (car x)))))
(define (cdaadr x) (cdr (car (car (cdr x)))))
(define (cdadar x) (cdr (car (cdr (car x)))))
(define (cdaddr x) (cdr (car (cdr (cdr x)))))
(define (cddaar x) (cdr (cdr (car (car x)))))
(define (cddadr x) (cdr (cdr (car (cdr x)))))
(define (cdddar x) (cdr (cdr (cdr (car x)))))
(define (cddddr x) (cdr (cdr (cdr (cdr x)))))

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

(define (string->symbol str)
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

(define (char? x)
  (and (string? x)
       (= (string-length x) 1)))

(define (char<=? c1 c2)
  (js-op c1 "<=" c2))

(define (char-numeric? c)
  (and (char<=? #\0 c) (char<=? c #\9)))

(define (char-alphabetic? c)
  (or (and (char<=? #\a c) (char<=? c #\z))
      (and (char<=? #\A c) (char<=? c #\Z))))

(define (string-ref s i)
  ((js-quote "function(s,i){return s[i];}") s i))

(define (substring str start end)
  ((.. str substr) start (- end start)))

(define (string-append & args)
  (fold-left (lambda (a b) (js-op a "+" b)) "" args))

(define string string-append)

(define (append & args)
  (letrec ((recur (lambda (ls)
		    (cond ((null? (cdr ls))
			   (car ls))
			  ((null? (car ls))
			   (recur (cdr ls)))
			  (else
			   (cons (caar ls) (recur (cons (cdar ls) (cdr ls)))))))))
    (if (null? args)
	'()
	(recur args))))

(define (map f l)
  (fold-right (lambda (x xs) (cons (f x) xs)) l '()))

(define (memq x l)
  (cond ((null? l)
	 #f)
	((eq? (car l) x)
	 l)
	(else
	 (memq x (cdr l)))))

(define (char-whitespace? c)
  (memq c '(#\space #\newline #\ht)))

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

(define (assoc x l)
  (letrec ((recur (lambda (l)
		    (cond ((null? l)
			   #f)
			  ((equal? (caar l) x)
			   (car l))
			  (else
			   (recur (cdr l)))))))
    (recur l)))

(define (display-to-string x)
  (cond ((null? x)
	 "()")
	((number? x)
	 (number->string x))
	((string? x)
	 x)
	((symbol? x)
	 (symbol->string x))
	((eq? x #t)
	 "#t")
	((eq? x #f)
	 "#f")
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

(define (display x)
  ((js-quote "print") (display-to-string x)))

(define (newline x)
  ((js-quote "print") "\n"))
