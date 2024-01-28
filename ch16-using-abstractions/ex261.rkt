#lang racket
(require test-engine/racket-tests)


(define-struct IR [name price])
;; An IR is a structure:
;;   (make-IR String Number)
;; An Inventory is one of:
;; – '()
;; – (cons IR Inventory)
(define IR-example0
  (make-IR "object 0" 0))
(define IR-example1
  (make-IR "object 1" 1))
(define IR-example2
  (make-IR "object 2" 2))
(define IR-example3
  (make-IR "object 3" 5))
(define inventory-example (list IR-example0 IR-example1 IR-example2 IR-example3))


;; Inventory -> Inventory
;; creates an Inventory from an-inv for all
;; those items that cost less than a dollar

(check-expect (extract '()) '())
(check-expect (extract inventory-example) (list IR-example0 IR-example1))

(define (extract an-inv)
  (cond [(empty? an-inv) '()]
        [else (cond [(<= (IR-price (car an-inv)) 1.0)
                     (cons (car an-inv) (extract (cdr an-inv)))]
                    [else (extract (cdr an-inv))])]))

;; Inventory -> Inventory
;; extract.v2 works just like extract

(check-expect (extract.v2 '()) (extract '()))
(check-expect (extract.v2 inventory-example) (extract inventory-example))

(define (extract.v2 an-inv)
  (cond
    ;; if given an empty inv, return an empty list
    ;; because no item is under a dollar
    [(empty? an-inv) '()]
    [else
     ;; else, first extract all items that are under a
     ;; dollar from cdr of an-inv
     (local ((define under-one-dollar/cdr (extract.v2 (cdr an-inv))))
       (cond [(<= (IR-price (car an-inv)) 1.0)
              ;; then, if car of an-inv is under a dollar,
              ;; cons it onto all items that are under a
              ;; dollar from cdr of an-inv
              (cons (car an-inv) under-one-dollar/cdr)]
             [else under-one-dollar/cdr]))]))

(test)
