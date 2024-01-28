;; sample problem 4.6.0
;; the state of tax land has created a three-state sales tax
;; cope with its budget deficit. inexpensive items, those costing less than $1,000,
;; are not taxed. luxury items, with a price of more than $10,000, are taxed at the
;; rate of right percent (8.00%). everything in between comes with a five percent
;; (5.00%) markup.
;;
;; design a function for a cash register that, given the price of an item, computes
;; the sales tax.

#lang racket
(require test-engine/racket-tests)

;; a Price falls into one of three intervals:
;; - [0, 1000) | low-price tier
;; - [1000, 10000) | mid-price tier
;; - 100000 and above | luxury-price tier
;; interpretation: the price of an item

(define mid-p 1000)
(define luxury-p 10000)

;; Price -> Number
;; computes the amount of tax charged for p
(check-within (sales-tax 0) 0 0.01)
(check-within (sales-tax 537) 0 0.01)
(check-within (sales-tax 1000) (* 1000 0.05) 0.01)
(check-within (sales-tax 1282) (* 1282 0.05) 0.01)
(check-within (sales-tax 10000) (* 10000 0.08) 0.01)
(check-within (sales-tax 12017) (* 12017 0.08) 0.01)

(define (sales-tax p)
  (cond [(and (<= 0 p) (< p mid-p)) 0]
        [(and (<= mid-p p) (< p luxury-p)) (* 0.05 p)]
        [(>= p luxury-p) (* 0.08 p)]))

(test)
