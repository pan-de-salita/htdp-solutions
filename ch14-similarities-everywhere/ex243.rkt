#lang racket
(require test-engine/racket-tests)

(define (f x) x)

(cons f '())
;; is a value. returns a list of f.
(check-expect (cons f '()) `(,f))

(f f)
;; is a value. returns f itself.
(check-expect (f f) f)

(cons f (cons 10 (cons (f 10) '())))
;; is a value. returns `(f 10 10).
