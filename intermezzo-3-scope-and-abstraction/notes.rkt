#lang racket
(require test-engine/racket-tests
         lang/posn)

(define-struct phone [area switch four])

(define (extract-area-codes a-phone)
  (match a-phone
    [(cons (phone area-code 664 9993) '()) (list area-code)]
    [(cons (phone area-code 664 9993) tail)
     (cons area-code (extract-area-codes tail))]))
