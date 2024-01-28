#lang racket
(require test-engine/racket-tests)

;; a List-of-Strings is one of the following:
;; - '()
;; - (cons String List-of-Strings)
;; i.e.: a list of characters with no spaces in-between

(define list-of-string-example-0
  '())
(define list-of-string-example-1
  (cons "a"
        (cons "b" '())))
(define list-of-string-example-2
  (cons "ab"
        (cons "cd"
              (cons "ef" '()))))

;; List-of-Strings -> String
;; concatenates all strings in a List-of-Strings into one
;; long string
(check-expect (cat '()) "")
(check-expect (cat (cons "a" (cons "b" '()))) "ab")
(check-expect
  (cat (cons "ab" (cons "cd" (cons "ef" '()))))
  "abcdef")

(define (cat a-list-of-strings)
  (cond
    [(empty? a-list-of-strings) ""]
    [else
     (string-append (first a-list-of-strings)
                    (cat (rest a-list-of-strings)))]))

(test)
