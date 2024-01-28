#lang racket
(require test-engine/racket-tests)

;; [List-of [List-of String]] -> [List-of Number]
;; determines the number of strings per item in
;; a list of list of strings

(define (words-on-line l-l-s)
  (for/list ([l-s l-l-s])
    (match l-s
      [(? list?) (length l-s)])))

(define input
  '(("this" "line" "has" "5" "strings")
    ("here" "only" "3")
    ()
    ("here" "2")))
(define output
  '(5 3 0 2))

(check-expect (words-on-line input) output)

(test)
