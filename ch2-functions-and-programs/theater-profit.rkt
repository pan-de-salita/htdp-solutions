;; Exercise 27. Our solution to the sample problem contains several constants in the
;; middle of functions. As One Program, Many Definitions already points out, it is best
;; to give names to such constants so that future readers understand where these
;; numbers come from. Collect all definitions in DrRacket's definitions area and change
;; them so that all magic numbers are refactored into constant definitions.

#lang racket
(require racket/trace)
(require test-engine/racket-tests)

(define BASE-TICKET-PRICE 5)
(define BASE-ATTENDEES 120)
(define ATTENDEE-CHANGE 15)
(define PRICE-CHANGE 0.1)
(define FIXED-COST-PER-PERFORMANCE 180)
(define VARIABLE-COST-PER-ATTENDEE 0.04)

(define (profit ticket-price)
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
  (+ FIXED-COST-PER-PERFORMANCE
     (* VARIABLE-COST-PER-ATTENDEE
        (attendees ticket-price))))
