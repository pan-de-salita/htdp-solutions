#lang racket
(require test-engine/racket-tests)

;; an S-expr is one of:
;; - Atom
;; - Sl

;; an SL is one of:
;; - '()
;; - (cons S-expr SL)

;; an Atom is one of:
;; - Number
;; - String
;; - Symbol

;; X -> Boolean
;; checks if X is an Atom

(check-expect (atom? 0) #t)
(check-expect (atom? "0") #t)
(check-expect (atom? 'a) #t)
(check-expect (atom? '()) #f)
(check-expect (atom? '(0 . 'a)) #f)
(check-expect (atom? #f) #f)

(define (atom? x)
  (local ((define atom-predicates
            (list number? string? symbol?)))
    (ormap (lambda (proc) (proc x)) atom-predicates)))

(test)
