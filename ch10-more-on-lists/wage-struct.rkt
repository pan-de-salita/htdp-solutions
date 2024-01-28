#lang racket
(require test-engine/racket-tests)

(define WAGE/H 14)

(define-struct work [employee rate hours])
;; a (piece of) Work is a structure:
;;     (make-work String Number Number)
;; i.e. (make-work n r h) combines the name n
;; with the pay rate r and the number of hours h

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

;; Low -> List-of-numbers
;; computes the weekly wages for the given records

(check-expect (wage*.v2 LOW-EX0) '())
(check-within (wage*.v2 LOW-EX1)
              (cons (wage.v2 (first LOW-EX1))
                    (wage*.v2 (rest LOW-EX1)))
              0.001)

(define (wage*.v2 low)
  (cond [(empty? low) low]
        [else (cons (wage.v2 (first low))
                    (wage*.v2 (rest low)))]))

;; Work -> ???
;; a template for processing elements of Work
(check-within (wage.v2 (make-work "Robby" 11.95 39))
              (* 11.95 39) 0.001)

(define (wage.v2 w)
  (* (work-rate w) (work-hours w)))

;; Number Number -> Number
;; computes the wage for h hours of work according
;; to given hourly rate r


(define (wage r h)
  (* r h))

(test)
