#lang racket
(require test-engine/racket-tests)
(require lang/posn)

;;; CONSTANTS ----------------------------------------------

;;; DATA DEFINITIONS ---------------------------------------

;; a List-of-Posns is one of:
;; - '()
;; - (cons Posn List-of-Posns)

;;; FUNCTIONS ----------------------------------------------

;; List-of-Posns -> Number
;; returns the sum of all x-coordinates in a List-of-Posns
(check-expect (sum '()) 0)
(check-expect (sum (cons (make-posn 1 2) '())) 1)
(check-expect (sum (cons (make-posn 1 2) (cons (make-posn 2 1) '()))) 3)

(define (sum lop)
  (cond [(empty? lop) 0]
        [else
         (+ (posn-x (first lop)) (sum (rest lop)))]))

;;; APPLICATION --------------------------------------------

(test)
