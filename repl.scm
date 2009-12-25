(define (read-eval-print str)
  (display-to-string ((js-quote "(function (x) { with (window) { return eval (x); } })")
		      (toplevel-compile (read-from-string str) '()))))
