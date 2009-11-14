(assert #t)
(assert (not #f))
(assert (not (not #t)))

(assert (eq? '() '()))
(assert (eq? 1 1))
;;(assert (eq? 'x 'x))

(assert (null? '()))
(assert (not (null? 0)))
(assert (not (null? "")))
(assert (not (null? '(x))))

(assert (< 0 1))
(assert (< -1 1))
(assert (not (< 1 1)))
(assert (not (< 1 0)))
