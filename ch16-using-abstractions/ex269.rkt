#lang racket
(require test-engine/racket-tests)

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

(define UNIT-OF-ACCOUNT 100)

;; Number [List-of Inventory-Record] -> [List-of Inventory-Record]
;; produces a list of Inventory-Records whose sales price is below
;; a unit of account

(check-expect (eliminate-expensive UNIT-OF-ACCOUNT '()) '())
(check-expect (eliminate-expensive UNIT-OF-ACCOUNT (list product0)) (list product0))
(check-expect (eliminate-expensive UNIT-OF-ACCOUNT (list product1)) '())
(check-expect
 (eliminate-expensive UNIT-OF-ACCOUNT (list product0 product1 product2 product3))
 (list product0 product3))

(define (eliminate-expensive unit-of-account l-inv-record)
  (local (;; Number -> Boolean
          ;; checks if the sales price of an Inventory Record is below
          ;; a unit of account
          (define (not-expensive? inv-record)
            (< (inv-record-sales-price inv-record) unit-of-account)))
    (filter not-expensive? l-inv-record)))

;; String [List-of Inventory-Record] -> [List-of Inventory-Record]
;; returns a list of Inventory-Records that do not use the same name
;; as recall-item

(check-expect (recall "book" '()) '())
(check-expect (recall "book" (list product0)) '())
(check-expect (recall "book" (list product1)) (list product1))
(check-expect
 (recall "book" (list product0 product1 product2 product3))
 (list product1 product2 product3))

(define (recall recall-item l-inv-records)
  (local (;; Inventory-Record -> Boolean
          ;; checks if an Inventory-Record is not a recall item
          (define (not-recall-item? inv-record)
            (not (string=? recall-item (inv-record-name inv-record)))))
    (filter not-recall-item? l-inv-records)))

;; [List-of String] [List-of String] -> [List-of String]
;; returns a list containing the strings in l-string-b that also
;; figure in l-string-a

(check-expect (selection '() '()) '())
(check-expect (selection (list "a") (list "a")) (list "a"))
(check-expect (selection (list "a") (list "b")) '())
(check-expect
 (selection (list "a" "b" "c")
            (list "b" "c" "d"))
 (list "b" "c"))

(define (selection l-string-a l-string-b)
  (local (;; String -> Boolean
          ;; checks if a-string figures in a list of strings
          (define (shared-string? a-string)
            (not (boolean? (member a-string l-string-a)))))
    (filter shared-string? l-string-b)))

(test)
