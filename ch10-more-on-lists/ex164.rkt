#lang racket
(require test-engine/racket-tests)

(define $->€-RATE 0.92)

;; Number -> Number
;; converts a US$ amount to a € amount
(check-within ($->€ 1) 0.92 0.001)
(check-within ($->€ 50) 46.00 0.001)

(define ($->€ $)
  (* $ $->€-RATE))

;; List-of-numbers -> List-of-numbers
;; converts a list of US$ amounts lo$ to
;; € amounts
(check-expect (convert-euro '()) '())
(check-within (convert-euro (cons 1 (cons 50 '())))
              (cons 0.92 (cons 46.00 '())) 0.001)

(define (convert-euro lo$)
  (cond [(empty? lo$) lo$]
        [else (cons ($->€ (first lo$)) (convert-euro (rest lo$)))]))

;; Number List-of-numbers -> List-of-numbers
;; converts a list of US$ amounts lo$ to
;; € amounts according to given exchange
;; rate er
(check-expect (convert-euro* 1 '()) '())
(check-within (convert-euro* 1 (cons 1 (cons 50 '())))
              (cons 1 (cons 50 '())) 0.001)

(define (convert-euro* er lo$)
  (cond [(empty? lo$) lo$]
        [else (cons (* er (first lo$)) (convert-euro* er (rest lo$)))]))

(test)
