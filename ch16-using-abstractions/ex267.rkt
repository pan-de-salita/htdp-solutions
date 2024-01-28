#lang racket
(require test-engine/racket-tests
         lang/posn)

;; [List-of Number] -> [List-of Number]
;; converts a list of US$ amounts into a list
;; of € amounts

(define EXCHANGE-RATE 1.06)

(check-expect (convert-euro '()) '())
(check-within (convert-euro (list 1.0)) (list 1.06) 0.001)
(check-within
 (convert-euro (list 1.0 45.33 12.72 8))
 (map (lambda (x) (* x EXCHANGE-RATE)) (list 1.0 45.33 12.72 8))
 0.001)

(define (convert-euro l-dollar)
  (local (;; Number -> Number
          ;; converts a single US$ amount into a € amount
          (define (dollar->euro dollar) (* dollar EXCHANGE-RATE)))
    (map dollar->euro l-dollar)))

;; [List-of Number] -> [List-of Number]
;; converts a list of Fahrenheit measurements into
;; a list of Celsius measurements

(check-expect (convert-FC '()) '())
(check-within (convert-FC (list 32.0)) (list 0) 0.001)
(check-within
 (convert-FC (list 32.0 342.12 -2))
 (map (lambda (x) (* (- x 32) 5/9)) (list 32.0 342.12 -2))
 0.001)

(define (convert-FC l-fahrenheit)
  (local (;; Number -> Number
          ;; converts a Fahrenheit measurement into
          ;; Celcius measurement
          (define (fahrenheit->celcius f) (* (- f 32) 5/9)))
    (map fahrenheit->celcius l-fahrenheit)))

;; [List-of Posn] -> [List-of [List-of Number Number]]
;; translates a list of Posns into a list of list of pairs of numbers

(check-expect (translate '()) '())
(check-expect (translate (list (make-posn 0 0))) (list (list 0 0)))
(check-expect
 (translate (list (make-posn 0 0) (make-posn 1 2) (make-posn 3 4)))
 (list (list 0 0) (list 1 2) (list 3 4)))

(define (translate l-posn)
  (local (;; Posn -> [List-of Number Number]
          ;; translates a Posn into a pair of numbers
          (define (posn->pair a-posn)
            (list (posn-x a-posn) (posn-y a-posn))))
    (map posn->pair l-posn)))

;; [List-of Posn] -> [List-of Pair]
;; translates a list of Posns into a list of pairs of numbers
;; (build-in data type)

(check-expect (translate/pair '()) '())
(check-expect (translate/pair (list (make-posn 0 0))) (list (cons 0 0)))
(check-expect
 (translate/pair (list (make-posn 0 0) (make-posn 1 2) (make-posn 3 4)))
 (list (cons 0 0) (cons 1 2) (cons 3 4)))

(define (translate/pair l-posn)
  (local (;; Posn -> Pair
          ;; translates a Posn into a pair of numbers (built-in data type)
          (define (posn->pair a-posn)
            (cons (posn-x a-posn) (posn-y a-posn))))
    (map posn->pair l-posn)))

(test)
