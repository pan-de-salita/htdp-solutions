#lang racket

;; a List-of-Booleans is one of:
;; - '()
;; - (cons Boolean List-of-Booleans)
;; i.e. a list of survey responses

(define list-of-booleans-example
  (cons #f
        (cons #t
              (cons #f
                    (cons #f '())))))
