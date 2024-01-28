;; Exercise 28. Determine the potential profit for these ticket prices: $1, $2, $3, $4,
;; and $5. Which price maximizes the profit of the movie theater? Determine the best
;; ticket price to a dime. Use the given function.

#lang racket
(require racket/trace)
(require test-engine/racket-tests)

(define BASE-TICKET-PRICE 5)
(define BASE-ATTENDEES 120)
(define ATTENDEE-CHANGE 15)
(define PRICE-CHANGE 0.1)
(define FIXED-COST-PER-PERFORMANCE 180)
(define VARIABLE-COST-PER-ATTENDEE 0.04)

(define (profit price)
  ;; Calculates revenue
  (- (* (+ BASE-ATTENDEES
           (* (/ ATTENDEE-CHANGE PRICE-CHANGE)
              (- BASE-TICKET-PRICE price)))
        price)
  ;; Calculates cost
     (+ FIXED-COST-PER-PERFORMANCE
        (* VARIABLE-COST-PER-ATTENDEE
           (+ BASE-ATTENDEES
              (* (/ ATTENDEE-CHANGE PRICE-CHANGE)
                 (- BASE-TICKET-PRICE price)))))))

(check-within (profit 1) 511.2 0.1)
(check-within (profit 2) 937.2 0.1)
(check-within (profit 3) 1063.2 0.1)
(check-within (profit 4) 889.2 0.1)
(check-within (profit 5) 415.2 0.1)
(test)
