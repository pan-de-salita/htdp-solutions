;; Exercise 15. Define ==>. The fuction consumes two Boolean values, call them
;; sunny and friday. Its answer is #true if sunny is false or friday is true. Note
;; Logicians call this Boolean operation implication, and they use the notation sunny
;; => friday for this purpose.

#lang htdp/bsl
(require test-engine/racket-tests)

;; boolean boolean -> boolean
;; Purpose: Consumes two boolean values and applies implication
;; NOTE: Logical implication truth table:
;; #t | #t => #t
;; #t | #f => #f
;; #f | #t => #t
;; #f | #f => #t
(define (==> sunny friday)
  (or (not sunny) friday))

(check-expect (==> #t #t) #t)
(check-expect (==> #t #f) #f)
(check-expect (==> #f #t) #t)
(check-expect (==> #f #f) #t)
(test)
