#lang racket
(require test-engine/racket-tests)

;; Number -> Boolean
(define divisible-by-10?
  (lambda (x) (= (modulo x 10) 0)))

;; Number Number -> Boolean
(define smaller-than-5?
  (lambda (x) (< x 5)))

;; a Set is a function:
;;   [X -> Boolean]
;; i.e. produces #t if x belongs to a Set, #f otherwise

;; Set X -> Boolean
;; checks whether x occurrs within a-set

(define (member?/set a-set x)
  (a-set x))

;; [X -> Boolean] -> Set
;; creates a representation of a Set all items of which satisfies a predicate P

(check-expect (member?/set (mk-set odd?) 0) #f)
(check-expect (member?/set (mk-set odd?) 1) #t)
(check-expect (member?/set (mk-set odd?) 2) #f)

(check-expect (member?/set (mk-set even?) 0) #t)
(check-expect (member?/set (mk-set even?) 1) #f)
(check-expect (member?/set (mk-set even?) 2) #t)

(check-expect (member?/set (mk-set divisible-by-10?) 10) #t)
(check-expect (member?/set (mk-set divisible-by-10?) 20) #t)
(check-expect (member?/set (mk-set divisible-by-10?) 21) #f)

(check-expect (member?/set (mk-set smaller-than-5?) 0) #t)
(check-expect (member?/set (mk-set smaller-than-5?) 1) #t)
(check-expect (member?/set (mk-set smaller-than-5?) 5) #f)

(define (mk-set P)
  (lambda (x) (P x)))

;; Set X -> Set
;; adds an-element to a Set, creating a new Set

(check-expect (member?/set (add-element 10 (mk-set odd?)) 10) #t)
(check-expect (member?/set (add-element 10 (mk-set even?)) 10) #t)
(check-expect (member?/set (add-element 21 (mk-set divisible-by-10?)) 21) #t)
(check-expect (member?/set (add-element 10 (mk-set smaller-than-5?)) 10) #t)

(define (add-element an-element a-set)
  (lambda (x)
    (or (member?/set a-set x)
        (equal? an-element x))))

;; Set Set -> Set
;; combines the elements of two Sets, creating a new Set

(check-expect (member?/set (union (mk-set odd?) (mk-set smaller-than-5?)) 1) #t)
(check-expect (member?/set (union (mk-set odd?) (mk-set smaller-than-5?)) 7) #t)
(check-expect (member?/set (union (mk-set even?) (mk-set smaller-than-5?)) 2) #t)
(check-expect (member?/set (union (mk-set even?) (mk-set smaller-than-5?)) 8) #t)
(check-expect (member?/set (union (mk-set divisible-by-10?) (mk-set smaller-than-5?)) 0) #t)
(check-expect (member?/set (union (mk-set divisible-by-10?) (mk-set smaller-than-5?)) 10) #t)

(define (union set-a set-b)
  (lambda (x)
    (or (member?/set set-a x)
        (member?/set set-b x))))

;; Set Set -> Set
;; collects all elements common to two Sets, creating a new Set

(check-expect (member?/set (intersect (mk-set odd?) (mk-set smaller-than-5?)) 1) #t)
(check-expect (member?/set (intersect (mk-set odd?) (mk-set smaller-than-5?)) 7) #f)
(check-expect (member?/set (intersect (mk-set even?) (mk-set smaller-than-5?)) 2) #t)
(check-expect (member?/set (intersect (mk-set even?) (mk-set smaller-than-5?)) 8) #f)
(check-expect (member?/set (intersect (mk-set divisible-by-10?) (mk-set smaller-than-5?)) 0) #t)
(check-expect (member?/set (intersect (mk-set divisible-by-10?) (mk-set smaller-than-5?)) 10) #f)

(define (intersect set-a set-b)
  (lambda (x)
    (and (member?/set set-a x)
         (member?/set set-b x))))

(test)
