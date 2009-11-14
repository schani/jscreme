(defmacro define (name value)
  (if (pair? name)
      (let ((name (car name))
	    (args (cdr name)))
	(list 'js-define name (list 'lambda args value)))
      (list 'js-define name value)))

(define (error & args)
  ((js-quote "function (x){throw x;}") args))

(define (assert x)
  (if x
      #t
      (error 'assertion-failed)))

(define (not x)
  (if x #f #t))

(define (eq? a b)
  (js-op a "==" b))

(define (null? x)
  (eq? x '()))

(define (< a b)
  (js-op a "<" b))

(define (car x)
  (.. x car))

(define (cdr x)
  (.. x cdr))

(define (cons a b)
  (js-object (car a) (cdr b)))

(define (vector-length v)
  (.. v length))

(load "utils.scm")

(define (+ & args)
  (fold-left (lambda (a b) (js-op a "+" b)) 0 args))

(define (- x & rest)
  (js-op x "-" (apply + rest)))

(define (listify-vector vec start)
  (letrec ((recur (lambda (i l)
		    (if (< i start)
			l
			(recur (js-op i "-" 1) (cons (vector-ref vec i) l))))))
    (recur (js-op (vector-length vec) "-" 1) '())))

(define (list & args)
  args)

(define (string-append & args)
  (fold-left (lambda (a b) (js-op a "+" b)) "" args))

(define (map f l)
  (fold-right (lambda (x xs) (cons (f x) xs)) l '()))

(define (length l)
  (fold-left (lambda (count x) (+ count 1)) 0 l))
