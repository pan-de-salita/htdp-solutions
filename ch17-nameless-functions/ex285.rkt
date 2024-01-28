#lang racket
(require test-engine/racket-tests
         lang/posn)

;; [List-of Number] -> [List-of Number]
;; converts a list of US$ amounts to a list of € amounts
;; based on an exchange rate of US$1.06 per €

(check-expect (convert-euro '()) '())
(check-within (convert-euro '(1)) '(1.06) 0.001)
(check-within (convert-euro '(1 2 3)) '(1.06 2.12 3.18) 0.001)

(define DOLLAR-PER-EURO 1.06)

(define (convert-euro l-dollar)
  (map (lambda (dollar) (* dollar DOLLAR-PER-EURO)) l-dollar))

;; [List-of Number] -> [List-of Number]
;; converts a list of Fahrenheit measurements to
;; a list of Celsius measurements

(check-expect (convertFC '()) '())
(check-within (convertFC '(1)) '(-155/9) 0.001)
(check-within (convertFC '(1 2 3)) '(-155/9 -50/3 -145/9) 0.001)

(define (convertFC l-f)
  (map (lambda (f) (* (- f 32) 5/9)) l-f))

;; [List-of Posn] -> [List-of [List-of Number Number]]
;; translates a [List-of Posn] into a [List-of [List-of Number Number]]

(check-expect (translate '()) '())
(check-expect (translate (list (make-posn 0 1))) (list (list 0 1)))
(check-expect (translate (list (make-posn 0 1) (make-posn 2 3))) (list (list 0 1) (list 2 3)))

(define (translate l-posn)
  (map (lambda (posn) (list (posn-x posn) (posn-y posn))) l-posn))

;; [List-of Posn] -> [List-of Pair]
;; translates a [List-of Posn] into a [List-of Pair]

(check-expect (translate-into-pair '()) '())
(check-expect (translate-into-pair (list (make-posn 0 1))) (list '(0 . 1)))
(check-expect (translate-into-pair (list (make-posn 0 1) (make-posn 2 3))) (list '(0 . 1) '(2 . 3)))

(define (translate-into-pair l-posn)
  (map (lambda (posn) (cons (posn-x posn) (posn-y posn))) l-posn))

(test)
