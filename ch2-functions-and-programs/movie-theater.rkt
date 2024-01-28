#lang racket
(require racket/trace)
(require test-engine/racket-tests)

(define ORIGINAL-TICKET-PRICE 5)
(define ORIGINAL-NUMBER-OF-ATTENDEES 120)
(define FIXED-COST-PER-PERFORMANCE 180)
(define VARIABLE-COST-PER-ATTENDEE 0.04)

(define (profit ticket-price)
  (- (revenue ticket-price)
     (cost ticket-price)))

(define (attendees ticket-price)
  (- ORIGINAL-NUMBER-OF-ATTENDEES
     (* (/ (- ticket-price ORIGINAL-TICKET-PRICE) 0.1)
        15)))

(define (revenue ticket-price)
  (* ticket-price (attendees ticket-price)))

(define (cost ticket-price)
  (+ FIXED-COST-PER-PERFORMANCE
     (* VARIABLE-COST-PER-ATTENDEE
        (attendees ticket-price))))
