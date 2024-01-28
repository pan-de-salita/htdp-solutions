#lang racket
(require test-engine/racket-tests)
(require 2htdp/image)

(define-struct layer [color doll])
;; an RD (short for Russian doll) is one of:
;; - String
;; - (make-layer String RD)
(define RD-EXAMPLE-0 "red")
(define RD-EXAMPLE-1 (make-layer "green" "red"))
(define RD-EXAMPLE-2 (make-layer "yellow" (make-layer "green" "red")))

;; RD -> Number
;; how many dolls are part of an-rd
(check-expect (depth RD-EXAMPLE-0) 1)
(check-expect (depth RD-EXAMPLE-1) 2)
(check-expect (depth RD-EXAMPLE-2) 3)

(define (depth an-rd)
  (cond
    [(string? an-rd) 1]
    [(layer? an-rd)
     (add1 (depth (layer-doll an-rd)))]))

(test)
