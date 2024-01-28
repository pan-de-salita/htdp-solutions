#lang racket
(require test-engine/racket-tests)

;; [X] [X -> Number] [NEList-of X] -> X
;; finds the (first) item in lx that maximizes f
;; if (argmax f (list x-1 ... x-n)) == x-i,
;; then (>= (f x-i) (f x-1)), (>= (f x-1) (f x-2)), ...

;; in other words, argmin returns the (first) item
;; on lx that, when f is applied to it, yields the
;; largest numerical value

(check-expect (argmax add1 (list 1 2 3)) 3)
(check-expect (argmax string-length (list "x" "xy" "xyz")) "xyz")

;; [X] [X -> Number] [NEList-of X] -> X
;; finds the (first) item in lx that minimizes f
;; if (argmin f (list x-1 ... x-n)) == x-i,
;; then (<= (f x-i) (f x-1)), (<= (f x-1) (f x-2)), ...

;; similarly, argmin returns the (first) item on lx
;; that, when f is applied to it, yields the smallest
;; numerical value

(check-expect (argmin add1 (list 1 2 3)) 1)
(check-expect (argmin string-length (list "x" "xy" "xyz")) "x")

(test)
