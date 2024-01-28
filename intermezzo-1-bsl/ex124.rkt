#lang racket

;; 1.
(define PRICE 5)
(define SALES-TAX (* 0.08 PRICE))
(define TOTAL (+ PRICE SALES-TAX))

;; PRICE == 5
;; SALES-TAX == (* 0.08 PRICE) == (* 0.08 5) == 0.4
;; TOTAL == (+ PRICE SALES-TAX) == (+ 5 0.4) == 5.4

;; 2.
(define COLD-F 32)
(define COLD-C (fahrenheit->celcius COLD-F))
(define (fahrenheit->celsius f)
  (* 5/9 (- f 32)))

;; signals an error because fahrenheit->celsius hasn't been
;; defined before its application.

;; 3.
(define LEFT -100)
(define RIGHT 100)
(define (f x) (+ (* 5 (* expt 2)) 10))
(define f@LEFT (f LEFT))
(define f@RIGHT (f RIGHT))

;; doesn't signal an error because f was defined before its
;; application.
