#lang racket
(require test-engine/racket-tests)

;; a [Maybe X] is one of:
;; - #f
;; - X

;; a [Maybe String] is one of:
;; - #f
;; - String

;; a [Maybe [List-of String]] is one of:
;; - #f
;; - [List-of String]

;; a [List-of [Maybe String]] is one of:
;; - '()
;; - (cons [Maybe String] [List-of [Maybe String]])

;; String [List-of String] -> [Maybe [List-of String]]
;; returns the remainder of los starting with s.
;; #f otherwise.

(check-expect (occurs "a" '("b" "a" "d" "e")) '("d" "e"))
(check-expect (occurs "a" '("b" "c" "d")) #f)

(define (occurs s los)
  (cond [(empty? los) #f]
        [else (cond [(string=? s (car los)) (cdr los)]
                    [else (occurs s (cdr los))])]))

(test)
