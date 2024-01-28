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
(define IR-2 (make-IR "laptop" "fourth-hand" 5 100))

;; Number [List-of IR] -> [List-of IR]
;; returns a list of all IRs whose acquisiton price is below ua

(check-expect (eliminate-exp 3 '()) '())
(check-expect (eliminate-exp 3 (list IR-0 IR-1 IR-2)) (list IR-0 IR-1))
(check-expect (eliminate-exp 2 (list IR-0 IR-1 IR-2)) (list IR-0))
(check-expect (eliminate-exp 1 (list IR-0 IR-1 IR-2)) '())

(define (eliminate-exp ua l-ir)
  (filter (lambda (ir) (< (IR-acquisition-price ir) ua)) l-ir))

;; ----------------------------------------

(define L-NAME-0 '())
(define L-NAME-1 (list "a"))
(define L-NAME-2 (list "b"))
(define L-NAME-3 (list "a" "b"))
(define L-NAME-4 (list "b" "c"))
(define L-NAME-5 (list "a" "b" "c"))
(define L-NAME-6 (list "d" "b" "a"))

;; [List-of String] [List-of String] -> [List-of String]
;; selects all names from l-name-q that are also on l-name-p

(check-expect (selection L-NAME-0 L-NAME-1) '())
(check-expect (selection L-NAME-2 L-NAME-3) '("b"))
(check-expect (selection L-NAME-4 L-NAME-5) '("b" "c"))
(check-expect (selection L-NAME-5 L-NAME-6) '("b" "a"))

(define (selection l-name-p l-name-q)
  (filter (lambda (name) (not (boolean? (member name l-name-p)))) l-name-q))

(test)
