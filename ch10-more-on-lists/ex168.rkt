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
;; adds 1 to each y-coordinate in a List-of-Posns

(check-expect (translate '()) '())
(check-expect (translate (cons (make-posn 0 1) '())) (cons (make-posn 0 2) '()))
(check-expect (translate (cons (make-posn 1 2) (cons (make-posn 3 4) '())))
              (cons (make-posn 1 3) (cons (make-posn 3 5) '())))

(define (translate lop)
  (cond [(empty? lop) empty]
        [else
         (cons (translate-y (first lop)) (translate (rest lop)))]))

;; Posn -> Posn
;; adds 1 to the y-coordinate in a given Posn

(check-expect (translate-y (make-posn 1 2)) (make-posn 1 3))

(define (translate-y p)
  (make-posn (posn-x p) (+ (posn-y p) 1)))

;;; APPLICATION --------------------------------------------

(test)
