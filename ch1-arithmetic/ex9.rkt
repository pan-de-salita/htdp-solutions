;; Exercise 9. Add the following line to the definitions ares of DrRacket:
;;
;; (define in ...)
;;
;; The create an expression that converts the value of in to a non-nagetive number.
;; For a String, it determines how long the String is; for an image, it uses the area; for
;; a Number, it uses the absolute value; for #true it uses 10 and for #false 20. Hint:
;; Check out cond from the Prologue: How to Program (again).

#lang htdp/bsl
(require test-engine/racket-tests)
(require 2htdp/image)

;; string/image/number/boolean -> number
;; Purpose: Computes the following: for a string, it determines how long the string is;
;;                                  for an image, it uses the area;
;;                                  for a number, it uses the absoute value;
;;                                  for #true, it uses 10;
;;                                  for #false, it uses 20.
(define (in x)
  (cond ((string? x) (string-length x))
        ((image? x) (* (image-width x) (image-height x)))
        ((number? x) (abs x))
        ((eq? x #t) 20)
        ((eq? x #f) 10)
        (else "invalid input")))

(check-expect (in "string") 6)
(check-expect (in (rectangle 10 10 "solid" "black")) 100)
(check-expect (in -2) 2)
(check-expect (in #t) 20)
(check-expect (in #f) 10)
(check-expect (in (list 1 2)) "invalid input")
(test)
