#lang racket
(require test-engine/racket-tests)

(define-struct layer [color doll] #:transparent)
;; an RD.v2 (short of Russian doll) is one of:
;; - "doll"
;; - (make-layer String RD.v2)

;; Layer -> Number
;; measures the number of layers surrounding a Russian doll

(check-expect (depth (make-layer "red" "doll")) 1)

(define (depth a-doll)
  (match a-doll
    ["doll" 0]
    [(layer color inside) (add1 (depth inside))]))

(test)
