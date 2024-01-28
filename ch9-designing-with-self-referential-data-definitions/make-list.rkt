#lang htdp/bsl
(require test-engine/racket-tests)

;; an N is one of:
;; - 0
;; - (add1 N)
;; i.e.: counting numbers

;; a List-of-Strings is one of:
;; - '()
;; - (cons String List-of-Strings)
;; i.e.: a list of strings

;; N String -> List-of-Strings
;; creates a list of n copies of String
(check-expect (copier 1 "cat") (cons "cat" '()))
(check-expect (copier 3 "dog") (cons "dog" (cons "dog" (cons "dog" '()))))
(check-expect (copier 0 "mouse") '())

(define (copier given-n given-string)
  (cond [(zero? given-n) '()]
        [(positive? given-n)
         (cons given-string
               (copier (sub1 given-n) given-string))]))

;; N String -> List-of-Strings
;; alternative definition of copier
(check-expect (copier.v2 1 "cat") (cons "cat" '()))
(check-expect (copier.v2 3 "dog") (cons "dog" (cons "dog" (cons "dog" '()))))
(check-expect (copier.v2 0 "mouse") '())

(define (copier.v2 given-n given-string)
  (cond [(zero? given-n) '()]
        [else
         (cons given-string
               (copier (sub1 given-n) given-string))]))

;; applying both copier functions to 0.1 and "x" produces the error
;; cond: all question results were false. this is because 0.1 doesn't
;; allow either function to reach its base case.

(test)
