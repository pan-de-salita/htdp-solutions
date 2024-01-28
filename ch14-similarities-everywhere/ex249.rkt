#lang racket
(require test-engine/racket-tests)

;;;; functions and expressions used for solution:

(define (f x) x)
(cons f '()) ;; == (list f '())
(f f) ;; == f
(cons f (cons 10 (cons (f 10) '()))) ;; == (list f 10 10)

;;;; solution

#|

(cons f '())
==
(cons #<procedure:f> '())
==
(list #<procedure:f>)

(f f)
==
#<procedure:f>

(cons f (cons 10 (cons (f 10) '())))
==
(cons #<procedure:f> (cons 10 (cons (f 10) '())))
==
(cons #<procedure:f> (cons 10 (cons 10 '())))
==
(cons #<procedure:f> (cons 10 (list 10)))
==
(cons #<procedure:f> (list 10 10))
==
(list #<procedure:f> 10 10)

|#
