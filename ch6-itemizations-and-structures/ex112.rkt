#lang racket
(require test-engine/racket-tests)
(require lang/posn)
(require 2htdp/image)

;; Any -> Boolean
;; is a an element of the MissileOrNot collection
(check-expect (missile-or-not? #false) #true)
(check-expect (missile-or-not? (make-posn 9 2)) #true)
(check-expect (missile-or-not? "yellow") #false)
(check-expect (missile-or-not? #true) #false)
(check-expect (missile-or-not? 10) #false)
(check-expect (missile-or-not? empty-image) #false)

(define (missile-or-not? v)
  (if (not (or (false? v) (posn? v)))
      #false
      #true))

(test)
