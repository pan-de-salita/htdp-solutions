;; Exercise 30. Define constants for the price optimization program at the movie theater so
;; that the price sensitivity of attendance (15 people for every 10 cents) becomes a
;; computed constant.

#lang racket
(require racket/trace)
(require test-engine/racket-tests)

;; Constants
(define BASE-TICKET-PRICE 5)
(define BASE-ATTENDEES 120)
(define ATTENDEE-CHANGE 15)
(define PRICE-CHANGE 0.1)
(define PRICE-SENSITIVITY (/ ATTENDEE-CHANGE PRICE-CHANGE))
(define VARIABLE-COST-PER-ATTENDEE 1.5)

(define (profit ticket-price)
  (- (revenue ticket-price)
     (cost ticket-price)))

(define (attendees ticket-price)
  (- BASE-ATTENDEES
     (* (- ticket-price BASE-TICKET-PRICE)
        PRICE-SENSITIVITY)))

(define (revenue ticket-price)
  (* ticket-price (attendees ticket-price)))

(define (cost ticket-price)
  (* VARIABLE-COST-PER-ATTENDEE
     (attendees ticket-price)))
