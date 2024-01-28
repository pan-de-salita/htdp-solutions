#lang racket
(require test-engine/racket-tests)
(require 2htdp/image)
(require 2htdp/universe)

;;; CONSTANTS -----------------------------------------

(define WIDTH 500)
(define HEIGHT 300)
(define TEXT-SIZE 14)
(define TEXT-COLOR "white")
(define TEXT-X (/ WIDTH 2))
(define TEXT-Y (/ HEIGHT 2))

(define CANVAS (empty-scene WIDTH HEIGHT "dimgrey"))
(define PROCESSING (text "processing..." TEXT-SIZE TEXT-COLOR))

;;; DATA DEFINITIONS ----------------------------------

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

;; a PayState is one of:
;; - '()
;; - (cons Work/Paycheck PayState)
;; a list of Work and Paycheck instances
(define PAYSTATE-EX0 '())
(define PAYSTATE-EX1
  (cons (make-work "Candace" 14 50)
        (cons (make-work "Amy" 13 45)
              (cons (make-work "Matthew" 12.95 45)
                    (cons (make-work "Robby" 11.95 39) '())))))
(define PAYSTATE-EX2
  (cons (make-paycheck "Candace" (* 14 50))
        (cons (make-paycheck "Amy" (* 13 45))
              (cons (make-paycheck "Matthew" (* 12.95 45))
                    (cons (make-paycheck "Robby" (* 11.95 39)) '())))))
(define PAYSTATE-EX3
  (cons (make-paycheck "Candace" (* 14 50))
        (cons (make-paycheck "Amy" (* 13 45))
              (cons (make-work "Matthew" 12.95 45)
                    (cons (make-work "Robby" 11.95 39) '())))))

;;; FUNCTIONS -----------------------------------------

;; PayState -> PayState
;; main function
(define (payday ps)
  (big-bang ps
    [to-draw render-payment]
    [on-tick wage*.v3 1]
    [stop-when all-processed? display-payments]))

;; PayState -> Image
;; displays a message that the program is processing
;; the payments

(define (render-payment ps)
  (cond [(or (work? (first ps)) (empty? ps)) CANVAS]
        [else (place-image PROCESSING
                           TEXT-X TEXT-Y
                           (render-payment (rest ps)))]))

;; PayState -> Image
;; displays the list of payment for each employee

(check-expect (display-payments LOP-EX4)
              (place-image (lop->text LOP-EX4) TEXT-X TEXT-Y CANVAS))

(define (display-payments ps)
  (place-image (lop->text ps) TEXT-X TEXT-Y CANVAS))

;; PayCheck -> Image
;; converts an instance of a paycheck into displayable text

(check-expect (lop->text (cons (make-paycheck "Robby" (* 11.95 39)) '()))
              (above (text (string-append
                            "employee: " "Robby" " | "
                            "amount: " (number->string (* 11.95 39)) "\n")
                           TEXT-SIZE TEXT-COLOR)
                     empty-image))

(define (lop->text p)
  (cond [(empty? p) empty-image]
        [else (above (text (string-append
                            "employee: " (paycheck-employee (first p)) " | "
                            "amount: " (number->string (paycheck-amount (first p))) "\n")
                           TEXT-SIZE TEXT-COLOR)
                     (lop->text (rest p)))]))

;; PayState -> Lop
;; computes the weekly wages for the given records

(check-expect (wage*.v3 LOW-EX0) LOP-EX0)
(check-expect (wage*.v3 LOW-EX1) LOP-EX1)
(check-expect (wage*.v3 LOW-EX2) LOP-EX2)
(check-expect (wage*.v3 LOW-EX3) LOP-EX3)
(check-expect (wage*.v3 LOW-EX4) LOP-EX4)

(define (wage*.v3 ps)
  (cond [(or (paycheck? ps) (empty? ps)) ps]
        [else (cons (wage.v3 (first ps))
                    (wage*.v3 (rest ps)))]))

;; Work -> Paycheck
;; calculates the paycheck of a given employee

(check-expect (wage.v3 (make-work "Robby" 11.95 39))
              (make-paycheck "Robby" (* 11.95 39)))

(define (wage.v3 w)
  (make-paycheck (work-employee w)
                 (* (work-rate w) (work-hours w))))

;; PayState -> Boolean
;; checks if all paychecks have been processed

(check-expect (all-processed? PAYSTATE-EX0) #true)
(check-expect (all-processed? PAYSTATE-EX1) #false)
(check-expect (all-processed? PAYSTATE-EX2) #true)
(check-expect (all-processed? PAYSTATE-EX3) #false)

(define (all-processed? ps)
  (cond [(empty? ps) #true]
        [else (and (paycheck? (first ps)) (all-processed? (rest ps)))]))

;;; APPLICATION ---------------------------------------

(payday PAYSTATE-EX2)
(test)
