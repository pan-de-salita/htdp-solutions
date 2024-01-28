#lang racket
(require test-engine/racket-tests)
(require 2htdp/image)
(require 2htdp/universe)

;;; CONSTANTS ----------------------------------------------

;;; DATA DEFINITIONS ---------------------------------------

(define-struct employee [name num])
;; an Employee is a structure:
;;     (make-employee String Number)
;; i.e. (make-employee n num) combines an employee's
;; name n and their employee number num

(define-struct work [employee rate hours])
;; a (piece of) Work is a structure:
;;     (make-work Employee Number Number)
;; i.e. (make-work e r h) combines an employee's information e
;; with the pay rate r and the number of hours h

(define-struct paycheck [employee amount])
;; a Paycheck is a structure:
;;     (make-paycheck Employee Number)
;; i.e. (make-paycheck e a) combines an employee's information e
;; with their calculated total wage amount

;; a Low (short for list of Works) is one of:
;; - '()
;; - (cons Work Low)
;; i.e. an instance of Low represents the
;; hours worked for a number of employees
(define LOW-EX0 '())
(define LOW-EX1
  (cons (make-work (make-employee "Robby" 000) 11.95 39) '()))
(define LOW-EX2
  (cons (make-work (make-employee "Matthew" 001) 12.95 45)
        (cons (make-work (make-employee "Robby" 000) 11.95 39) '())))
(define LOW-EX3
  (cons (make-work (make-employee "Amy" 002) 13 45)
        (cons (make-work (make-employee "Matthew" 001) 12.95 45)
              (cons (make-work (make-employee "Robby" 000) 11.95 39) '()))))
(define LOW-EX4
  (cons (make-work (make-employee "Candace" 003) 14 50)
        (cons (make-work (make-employee "Amy" 002) 13 45)
              (cons (make-work (make-employee "Matthew" 001) 12.95 45)
                    (cons (make-work (make-employee "Robby" 000) 11.95 39) '())))))

;; a Lop (short for list of Paychecks) is one of:
;; - '()
;; - (cons Paycheck Lop)
;; i.e. an instance of Lop represents the
;; the total pay received by a number of employees
(define LOP-EX0 '())
(define LOP-EX1
  (cons (make-paycheck (make-employee "Robby" 000) (* 11.95 39)) '()))
(define LOP-EX2
  (cons (make-paycheck (make-employee "Matthew" 001) (* 12.95 45))
        (cons (make-paycheck (make-employee "Robby" 000) (* 11.95 39)) '())))
(define LOP-EX3
  (cons (make-paycheck (make-employee "Amy" 002) (* 13 45))
        (cons (make-paycheck (make-employee "Matthew" 001) (* 12.95 45))
              (cons (make-paycheck (make-employee "Robby" 000) (* 11.95 39)) '()))))
(define LOP-EX4
  (cons (make-paycheck (make-employee "Candace" 003) (* 14 50))
        (cons (make-paycheck (make-employee "Amy" 002) (* 13 45))
              (cons (make-paycheck (make-employee "Matthew" 001) (* 12.95 45))
                    (cons (make-paycheck (make-employee "Robby" 000) (* 11.95 39)) '())))))

;;; FUNCTIONS ----------------------------------------------

;; Low -> Lop
;; computes the weekly wages for the given records

(check-expect (wage*.v4 LOW-EX0) LOP-EX0)
(check-expect (wage*.v4 LOW-EX1) LOP-EX1)
(check-expect (wage*.v4 LOW-EX2) LOP-EX2)
(check-expect (wage*.v4 LOW-EX3) LOP-EX3)
(check-expect (wage*.v4 LOW-EX4) LOP-EX4)

(define (wage*.v4 low)
  (cond [(empty? low) low]
        [else (cons (wage.v4 (first low))
                    (wage*.v4 (rest low)))]))

;; Work -> Paycheck
;; calculates the paycheck of a given employee

(check-expect (wage.v4 (make-work (make-employee "Robby" 000) 11.95 39))
              (make-paycheck (make-employee "Robby" 000) (* 11.95 39)))

(define (wage.v4 w)
  (make-paycheck (work-employee w)
                 (* (work-rate w) (work-hours w))))

;;; APPLICATION --------------------------------------------

(test)
