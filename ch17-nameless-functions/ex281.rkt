#lang racket
(require 2htdp/image
         lang/posn
         test-engine/racket-tests)

;; Number -> Boolean
;; checks if a a-number is less than 10
(check-expect (<10? 4) #t)
(check-expect (<10? 10) #f)
(define <10?
  (lambda (a-number)
    (< a-number 10)))

;; ----------------------------------------

;; Number Number -> String
;; multiplies x and y and outputs the result as a String
(check-expect (multiply-into-string 2 3) "6")
(define multiply-into-string
  (lambda (x y)
    (number->string (* x y))))

;; ----------------------------------------

(define MESSAGE "---- zero-or-one ----\nexpected a natural number\ngiven: ")

;; Number -> Number
;; returns the following for a-nutural-number:
;; - 0 if it is even
;; - else 1
(check-error (zero-or-one 0) (string-append MESSAGE "0"))
(check-error (zero-or-one -1) (string-append MESSAGE "-1"))
(check-expect (zero-or-one 1) 1)
(check-expect (zero-or-one 2) 0)

(define zero-or-one
  (lambda (n)
    (cond [(>= 0 n) (error (string-append MESSAGE (number->string n)))]
          [else (if (even? n) 0 1)])))

;; ----------------------------------------

(struct IR [item price] #:transparent)

;; IR IR [IR -> IR]-> IR
;; compares two IRs by price according to given comparator
(check-expect (compare-ir (IR "book" 10) (IR "kindle" 100) >) (IR "kindle" 100))

(define compare-ir
  (lambda (ir-1 ir-2 cmp)
    (if (cmp (IR-price ir-1) (IR-price ir-2))
        ir-1
        ir-2)))

;; ----------------------------------------

(define DOT (circle 10 "solid" "red"))
(define SCENE (empty-scene 100 100))

;; Posn Image -> Image
;; adds a red dot at a given Posn to a given Image
(check-expect
 (add-dot (make-posn 50 50) SCENE)
 (place-image DOT 50 50 SCENE))

(define add-dot
  (lambda (a-posn an-image)
    (place-image DOT (posn-x a-posn) (posn-y a-posn) an-image)))

;; ----------------------------------------

(test)
