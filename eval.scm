(define (eval x env)
  ((js-quote "eval") (compile x *toplevel-bindings*)))

(define (interaction-environment)
  '())
