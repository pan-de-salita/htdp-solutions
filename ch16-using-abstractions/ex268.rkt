#lang racket
(require test-engine/racket-tests)

;;;; data and constant definitions

(define-struct inv-record [name des acq-price sales-price] #:transparent)
;; a Inventory-Record is a structure:
;;   (make-product String String Number Number)
;; i.e. the following of an inventory item:
;; - name
;; - description
;; - acquisition price
;; - recommended sales price
(define product0
  (make-inv-record "book" "hardbound" 1 2))
(define product1
  (make-inv-record "laptop" "broken" 2 150))
(define product2
  (make-inv-record "idea" "barely used" 2 150))
(define product3
  (make-inv-record "cup of coffee" "secondhand" 10 25))

;;;; function

;; [List-of Inventory-Record] [Number Number -> Boolean]
;; -> [List-of Inventory-Record]
;; sorts a list of Inventory-Records by the difference between
;; their acquisition and recommended sales prices

(check-expect (sort-by-profit-margin '() >=) '())
(check-expect (sort-by-profit-margin (list product0) >=) (list product0))
(check-expect
 (sort-by-profit-margin (list product0 product1) >)
 (list product1 product0))
(check-expect
 (sort-by-profit-margin (list product0 product1) <)
 (list product0 product1))
(check-expect
 (sort-by-profit-margin (list product0 product1 product2 product3) >=)
 (list product2 product1 product3 product0))
(check-expect
 (sort-by-profit-margin (list product0 product1 product2 product3) <=)
 (list product0 product3 product2 product1))

(define (sort-by-profit-margin l-inv-record cmp)
  (local (;; Inventory-Record -> Number
          ;; calculates the profit margin of an Inventory-Record
          (define (profit-margin-of inv-record)
            (abs (- (inv-record-acq-price inv-record)
                    (inv-record-sales-price inv-record))))
          ;; Inventory-Record Inventory-Record -> Boolean
          ;; compares the profit margins of two Inventory-Records
          (define (compare-profit-margin inv-r-a inv-r-b)
            (cmp (profit-margin-of inv-r-a) (profit-margin-of inv-r-b))))
    (sort l-inv-record compare-profit-margin)))

;;;; application

(test)
