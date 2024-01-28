#lang racket
(require test-engine/racket-tests)

;; a Son.L is one of:
;; - '()
;; - (cons Number Son.L)
;; Son is used when it applies to Son.L and Son.R

;; a Son.R is one of:
;; - '()
;; - (cons Number Son.R)
;; constraint: if is is a Son.R, no number occurs
;; twice in s

;; Number Son -> Boolean
;; is x in s
(check-expect (in? 1 (cons 1 '())) #t)
(check-expect (in? 1 (cons 1 (cons 1 '()))) #t)
(check-expect (in? 1 (cons 2 '())) #f)
(check-expect (in? 1 '()) #f)

(define (in? x s)
  (cond [(empty? s) #f]
        [(cons? s)
         (or (= (first s) x)
             (in? x (rest s)))]))

;; Number Son.L -> Son.L
;; adds x to s
(check-expect (set+.L 1 '()) (cons 1 '()))
(check-expect (set+.L 1 (cons 1 '())) (cons 1 (cons 1 '())))

(define (set+.L x s)
  (cons x s))

;; Number Son.R -> Son.R
;; adds x to s
(check-expect (set+.R 1 '()) (cons 1 '()))
(check-expect (set+.R 1 (cons 1 '())) (cons 1 '()))

(define (set+.R x s)
  (if (in? x s) s (cons x s)))

(test)
