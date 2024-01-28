#lang racket
(require test-engine/racket-tests)
(require lang/posn)

;; computes the Manhattan distance of a posn from origin 0
(check-expect (manhattan-distance (make-posn 11 12))
              (+ (posn-x (make-posn 11 12))
                 (posn-y (make-posn 11 12))))
(check-expect (manhattan-distance (make-posn 23 32))
              (+ (posn-x (make-posn 23 32))
                 (posn-y (make-posn 23 32))))
(check-expect (manhattan-distance (make-posn 100 3))
              (+ (posn-x (make-posn 100 3))
                 (posn-y (make-posn 100 3))))

(define (manhattan-distance point)
  (+ (posn-x point)
     (posn-y point)))

(test)

;; If only the output of the program was desired, using a "direct"
;; or "random walk" strategy wouldn't matter. If computational
;; efficiency is at stake, however, using the "direct" strategy
;; is possibly more efficient as it requires fewer steps to
;; reach a result.
