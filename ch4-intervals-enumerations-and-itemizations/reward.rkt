#lang htdp/bsl
(require test-engine/racket-tests)

;; a positive-number is a number greater than/equal to 0.

;; positive-number -> string
;; computes the reward level from the given score s.
(define (reward s)
  (cond [(<= 0 s 10) "bronze"]
        [(and (< 10 s) (>= 20 s)) "silver"]
        [else "gold"])) ;; complement of all previous conditions
