(define (fold-left f neut l)
  (if (null? l)
      neut
      (fold-left f (f neut (car l)) (cdr l))))

(define (fold-right f l neut)
  (if (null? l)
      neut
      (f (car l) (fold-right f (cdr l) neut))))

(define (reduce f l)
  (if (null? l)
      '()
      (fold-left f (car l) (cdr l))))
