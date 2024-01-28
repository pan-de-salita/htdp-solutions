#lang racket
(require test-engine/racket-tests)

;; [List-of Number] -> [List-of Number]
;; converts a list of $US amounts into
;; a list of € amounts

(check-within (currency->currency '(1 2 3) 1.06)
              (map (lambda (to-convert) (* to-convert 1.06)) '(1 2 3))
              0.001)

(define (currency->currency l-amounts conversion-rate)
  (for/list ([to-convert l-amounts])
    (* to-convert conversion-rate)))

(test)
