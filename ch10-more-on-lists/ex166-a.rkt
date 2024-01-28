#lang racket
(require test-engine/racket-tests)
(require 2htdp/image)
(require 2htdp/universe)

;;; CONSTANTS ----------------------------------------------

;;; DATA DEFINITIONS ---------------------------------------

(define-struct work [employee rate hours])
;; a (piece of) Work is a structure:
;;     (make-work String Number Number)
;; i.e. (make-work n r h) combines an employee's name n
;; with the pay rate r and the number of hours h

(define-struct paycheck [employee amount])
;; a Paycheck is a structure:
;;     (make-paycheck String Number)
;; i.e. (make-paycheck n a) combines an employee's name n
;; with their calculated total wage amount

;; a Low (short for list of Works) is one of:
;; - '()
;; - (cons Work Low)
;; i.e. an instance of Low represents the
;; hours worked for a number of employees
(define LOW-EX0 '())
(define LOW-EX1
  (cons (make-work "Robby" 11.95 39) '()))
(define LOW-EX2
  (cons (make-work "Matthew" 12.95 45)
        (cons (make-work "Robby" 11.95 39) '())))
(define LOW-EX3
  (cons (make-work "Amy" 13 45)
        (cons (make-work "Matthew" 12.95 45)
              (cons (make-work "Robby" 11.95 39) '()))))
(define LOW-EX4
  (cons (make-work "Candace" 14 50)
        (cons (make-work "Amy" 13 45)
              (cons (make-work "Matthew" 12.95 45)
                    (cons (make-work "Robby" 11.95 39) '())))))

;; a Lop (short for list of Paychecks) is one of:
;; - '()
;; - (cons Paycheck Lop)
;; i.e. an instance of Lop represents the
;; the total pay received by a number of employees
(define LOP-EX0 '())
(define LOP-EX1
  (cons (make-paycheck "Robby" (* 11.95 39)) '()))
(define LOP-EX2
  (cons (make-paycheck "Matthew" (* 12.95 45))
        (cons (make-paycheck "Robby" (* 11.95 39)) '())))
(define LOP-EX3
  (cons (make-paycheck "Amy" (* 13 45))
        (cons (make-paycheck "Matthew" (* 12.95 45))
              (cons (make-paycheck "Robby" (* 11.95 39)) '()))))
(define LOP-EX4
  (cons (make-paycheck "Candace" (* 14 50))
        (cons (make-paycheck "Amy" (* 13 45))
              (cons (make-paycheck "Matthew" (* 12.95 45))
                    (cons (make-paycheck "Robby" (* 11.95 39)) '())))))

;;; FUNCTIONS ----------------------------------------------

;; Low -> Lop
;; computes the weekly wages for the given records

(check-expect (wage*.v3 LOW-EX0) LOP-EX0)
(check-expect (wage*.v3 LOW-EX1) LOP-EX1)
(check-expect (wage*.v3 LOW-EX2) LOP-EX2)
(check-expect (wage*.v3 LOW-EX3) LOP-EX3)
(check-expect (wage*.v3 LOW-EX4) LOP-EX4)

(define (wage*.v3 low)
  (cond [(empty? low) low]
        [else (cons (wage.v3 (first low))
                    (wage*.v3 (rest low)))]))

;; Work -> Paycheck
;; calculates the paycheck of a given employee

(check-expect (wage.v3 (make-work "Robby" 11.95 39))
              (make-paycheck "Robby" (* 11.95 39)))

(define (wage.v3 w)
  (make-paycheck (work-employee w)
                 (* (work-rate w) (work-hours w))))

;;; APPLICATION --------------------------------------------

(test)
