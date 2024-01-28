#lang racket
(require test-engine/racket-tests)
(require 2htdp/image)

;;; data definitions
(define-struct layer [color doll])
;; an RD (short for Russian doll) is one of:
;; - String
;; - (make-layer String RD)
(define RD-EXAMPLE-0 "red")
(define RD-EXAMPLE-1 (make-layer "green" "red"))
(define RD-EXAMPLE-2 (make-layer "yellow" (make-layer "green" "red")))

;;; function definitions
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

;; RD -> String
;; lists the colors contained in an-rd
(check-expect (colors RD-EXAMPLE-0) "red")
(check-expect (colors RD-EXAMPLE-1) "green, red")
(check-expect (colors RD-EXAMPLE-2) "yellow, green, red")

(define (colors an-rd)
  (cond
    [(string? an-rd) an-rd]
    [(layer? an-rd)
     (string-append (layer-color an-rd) ", " (colors (layer-doll an-rd)))]))

;;; application
(test)
