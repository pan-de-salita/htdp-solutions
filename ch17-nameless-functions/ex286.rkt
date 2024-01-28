#lang racket
(require test-engine/racket-tests)

(define-struct IR [name description acquisition-price sales-price] #:transparent)
;; an IR is a structure:
;;   (make-IR String String Number Number)
;; i.e an inventory record (IR) specifies
;; - the name of an inventory item
;; - a description
;; - the acquisition price
;; - the recommended sales price
(define IR-0 (make-IR "book" "second-hand" 1 10))
(define IR-1 (make-IR "keyboard" "third-hand" 2 50))
(define IR-2 (make-IR "laptop" "fourth-hand" 3 100))

;; [List-of IR] -> [List-of IR]
;; sorts a [List-of IR] by the difference between
;; - acquisition price
;; - recommended sales price

(check-expect (sort-by-profit-margin '() >=) '())
(check-expect (sort-by-profit-margin (list IR-0) >=) (list IR-0))
(check-expect (sort-by-profit-margin (list IR-0 IR-1) >=) (list IR-1 IR-0))
(check-expect (sort-by-profit-margin (list IR-0 IR-2 IR-1) >=) (list IR-2 IR-1 IR-0))
(check-expect (sort-by-profit-margin (list IR-0 IR-2 IR-1) <=) (reverse (list IR-2 IR-1 IR-0)))

(define (sort-by-profit-margin l-ir cmp)
  (sort
   l-ir
   (lambda (ir-0 ir-1)
     (cmp (abs (- (IR-sales-price ir-0) (IR-acquisition-price ir-0)))
          (abs (- (IR-sales-price ir-1) (IR-acquisition-price ir-1)))))))

(test)
