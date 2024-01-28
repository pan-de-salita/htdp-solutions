#lang racket
(require test-engine/racket-tests)
(require lang/posn)

(check-member-of "green" "red" "yellow" "grey")
;; fails; no outcome matches the expected value

(check-within (make-posn #i1.0 #i1.1)
              (make-posn #i0.9 #i1.2)  0.01)
;; fails; outcome outside of allowable tolerance

(check-range #i0.9 #i0.6 #i0.8)
;; fails; outcome outside of allowable range

(check-random (make-posn (random 3) (random 9))
              (make-posn (random 9) (random 3)))
;; fails; outcome does not match expected result

(check-satisfied 4 odd?)
;; fails; outcome does not satisfy predicate
