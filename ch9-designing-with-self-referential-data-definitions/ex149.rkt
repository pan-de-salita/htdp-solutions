#lang racket
(require test-engine/racket-tests)
(require 2htdp/image)

;; an N is one of:
;; - 0
;; - (add1 N)
;; i.e.: counting numbers

;; a List-of-Strings is one of:
;; - '()
;; - (cons String List-of-Strings)
;; i.e.: a list of strings

;; N String -> List-of-Strings
;; creates a list of n copies of String.
(check-expect (copier 1 "cat") (cons "cat" '()))
(check-expect (copier 3 "dog") (cons "dog" (cons "dog" (cons "dog" '()))))
(check-expect (copier 0 "mouse") '())

;; NOTE: copier also functions when applied to Booleans or Images.
(check-expect (copier 2 #false) (cons #false (cons #false '())))
(check-expect (copier 3 empty-image) (cons empty-image (cons empty-image (cons empty-image '()))))

(define (copier given-n given-string)
  (cond [(zero? given-n) '()]
        [(positive? given-n)
         (cons given-string
               (copier (sub1 given-n) given-string))]))

(test)
