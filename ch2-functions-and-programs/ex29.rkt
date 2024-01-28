;; Exercise 29. After studying the costs of a show, the owner discovered several ways
;; of lowering the cost. As a result of these improvements, there is no longer a fixed
;; cost; a variable cost of $1.50 per attendee remains.
;;
;; Modify both programs to reflect this change. When the programs are modified, test
;; them again with ticket prices of $3, $4, and $5 and compare the results.

#lang racket
(require racket/trace)
(require test-engine/racket-tests)

;; Constants
(define BASE-TICKET-PRICE 5)
(define BASE-ATTENDEES 120)
(define ATTENDEE-CHANGE 15)
(define PRICE-CHANGE 0.1)
(define VARIABLE-COST-PER-ATTENDEE 1.5)

;; Calculates profit modularly
(define (profit-modular ticket-price)
  (- (revenue ticket-price)
     (cost ticket-price)))

(define (attendees ticket-price)
  (- BASE-ATTENDEES
     (* ATTENDEE-CHANGE
        (/ (- ticket-price BASE-TICKET-PRICE)
           PRICE-CHANGE))))

(define (revenue ticket-price)
  (* ticket-price (attendees ticket-price)))

(define (cost ticket-price)
  (* VARIABLE-COST-PER-ATTENDEE
     (attendees ticket-price)))

;; Calculates profit monolithicly
(define (profit-monolithic price)
  ;; Calculates revenue
  (- (* (+ BASE-ATTENDEES
           (* (/ ATTENDEE-CHANGE PRICE-CHANGE)
              (- BASE-TICKET-PRICE price)))
        price)
  ;; Calculates cost
     (* VARIABLE-COST-PER-ATTENDEE
        (+ BASE-ATTENDEES
           (* (/ ATTENDEE-CHANGE PRICE-CHANGE)
              (- BASE-TICKET-PRICE price))))))

;; Tests
;; (check-within (profit-modular 1) (profit-monolithic 1) 0.1)
;; (check-within (profit-modular 2) (profit-monolithic 2) 0.1)
(check-within (profit-modular 3) (profit-monolithic 3) 0.1)
(check-within (profit-modular 4) (profit-monolithic 4) 0.1)
(check-within (profit-modular 5) (profit-monolithic 5) 0.1)
(test)
