#lang racket

; Nelon -> Number
; determines the smallest
; number on l
(define (inf l)
  (cond
    [(empty? (rest l))
     (first l)]
    [else
     (if (< (first l)
            (inf (rest l)))
         (first l)
         (inf (rest l)))]))

; Nelon -> Number
; determines the smallest number on l
(define (inf.v2 l)
  (cond
    [(empty? (rest l)) (first l)]
    [else
     (local ((define smallest-in-rest (inf.v2 (rest l))))
       (if (< (first l) smallest-in-rest)
           (first l)
           smallest-in-rest))]))

(inf.v2 (list 5 4 3 2 1)) ;; 54 steps
(inf (list 5 4 3 2 1)) ;; 231 steps

#|
inf.v2 performs significantly faster. reason:

since the evaluation of a local expression evaluates the definitions
once before proceeding to the body, meaning (inf (rest l)) is evaluated
only once while the body of the local expression refers to the result twice.
|#
