#lang racket
(require test-engine/racket-tests)
(require lang/posn)

;;; CONSTANTS ----------------------------------------------

;;; DATA DEFINITIONS ---------------------------------------

;; a List-of-Posns is one of:
;; - '()
;; - (cons Posn List-of-Posns)

;;; FUNCTIONS ----------------------------------------------

;; List-of-Posns -> List-of-Posns
;; in a List-of-Posns p, retains only those Posns whose:
;; - x-coordinates are between 0 and 100
;; - y-coordinates are between 0 and 200

(check-expect (legal '()) '())
(check-expect (legal (cons (make-posn 0 1) '()))
              (cons (make-posn 0 1) '()))
(check-expect (legal (cons (make-posn 1 350)
                           (cons (make-posn 0 1)
                                 (cons (make-posn 44 45)
                                       (cons (make-posn -2 99)
                                             (cons (make-posn 101 -9) '()))))))
              (cons (make-posn 0 1)
                    (cons (make-posn 44 45) '())))

(define (legal lop)
  (cond [(empty? lop) empty]
        [else
         (if (legal-posn? (first lop))
             (cons (first lop) (legal (rest lop)))
             (legal (rest lop)))]))

;; Posn -> Boolean
;; checks if given Posn p contains:
;; - an x-coordinate between 0 and 100
;; - a y-coordinate between 0 and 200

(check-expect (legal-posn? (make-posn 0 1)) #true)
(check-expect (legal-posn? (make-posn 1 350)) #false)
(check-expect (legal-posn? (make-posn 44 45)) #true)
(check-expect (legal-posn? (make-posn -2 99)) #false)
(check-expect (legal-posn? (make-posn 101 -9)) #false)

(define (legal-posn? p)
  (and (<= 0 (posn-x p) 100)
       (<= 0 (posn-y p) 200)))

;;; APPLICATION --------------------------------------------

(test)
