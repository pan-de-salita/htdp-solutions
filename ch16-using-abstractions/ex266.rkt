#lang racket
(require test-engine/racket-tests)

((local ((define (f x) (+ x 3))
         (define (g x) (* x 4)))
   (if (odd? (f (g 1)))
       f
       g))
 2)
==
((local ((define (f-0 x) (+ x 3))
         (define (g-0 x) (* x 4)))
   (if (odd? (f-0 (g-0 1)))
       f-0
       g-0))
 2)
==
(define (f-0 x)
  (+ x 3))

(define (g-0 x)
  (* x 4))

((if (odd? (f-0 (g-0 1)))
     f-0
     g-0)
 2)
==
(define (f-0 x)
  (+ x 3))

(define (g-0 x)
  (* x 4))

((if (odd? (f-0 (* 1 4)))
     f-0
     g-0)
 2)
==
(define (f-0 x)
  (+ x 3))

(define (g-0 x)
  (* x 4))

((if (odd? (f-0 4))
     f-0
     g-0)
 2)
==
(define (f-0 x)
  (+ x 3))

(define (g-0 x)
  (* x 4))

((if (odd? (+ 4 3))
     f-0
     g-0)
 2)
==
(define (f-0 x)
  (+ x 3))

(define (g-0 x)
  (* x 4))

((if (odd? 7)
     f-0
     g-0)
 2)
==
(define (f-0 x)
  (+ x 3))

(define (g-0 x)
  (* x 4))

((if #t
     f-0
     g-0)
 2)
==
(define (f-0 x)
  (+ x 3))

(define (g-0 x)
  (* x 4))

(f-0 2)
==
(define (f-0 x)
  (+ x 3))

(define (g-0 x)
  (* x 4))

(+ 2 3)
==
(define (f-0 x)
  (+ x 3))

(define (g-0 x)
  (* x 4))

5
