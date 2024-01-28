#lang htdp/bsl
(require test-engine/racket-tests)

(define (next traffic-light-state)
  (cond [(string=? "red" traffic-light-state) "green"]
        [(string=? "green" traffic-light-state) "yellow"]
        [(string=? "yellow" traffic-light-state) "red"]))
