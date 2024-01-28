#lang racket
(require test-engine/racket-tests)

;; Number -> Number
;; converts from Fahrenheit to Celcius
(check-within (f->c 0) -17.7778 0.001)
(check-within (f->c 100) 37.7778 0.001)

(define (f->c temp)
  (* (- temp 32) 5/9))

;; List-of-numbers -> List-of-numbers
;; converts a list of Fahrenheit temperatures
;; lof to Celsius temperatures
(check-expect (f->c* '()) '())
(check-within (f->c* (cons 0 (cons 100 '())))
              (cons -17.7778 (cons 37.7778 '())) 0.001)

(define (f->c* lof)
  (cond [(empty? lof) lof]
        [else (cons (f->c (first lof)) (f->c* (rest lof)))]))

(test)
