;; Exercise 12. Define the function cvolume, which accepts the length of a side of an
;; equilateral cube and computes its volume. If you have time, consider defining
;; csurface, too.

#lang htdp/bsl
(require test-engine/racket-tests)

;; number -> number
;; Purpose: Accepts the length of a side of an equilateral cube and computes its volume
(define (cvolume clength)
  (* clength clength clength))

(check-expect (cvolume 2) 8)
(test)

;; number -> number
;; Purpose: Accepts the length of a side of an equilateral cube and computes its surface area
(define (csurface clength)
  (* (* clength clength) 6))

(check-expect (csurface 2) 24)
(test)
