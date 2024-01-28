#lang racket
(require test-engine/racket-tests)

;; Letter [List-of Letter] -> [List-of [List-of Letter]]
;; inserts a letter into all possible positions in a given Word

(check-expect (insert-everywhere "a" '()) '(("a")))
(check-expect (insert-everywhere "a" '("b")) '(("a" "b") ("b" "a")))
(check-expect (insert-everywhere "a" '("b" "c")) '(("a" "b" "c") ("b" "a" "c") ("b" "c" "a")))

(define (insert-everywhre a-letter a-word)
  (cond [(empty? a-word) (list (list a-letter))]
        [else (cons (cons a-letter a-word)
                    (prepend-everywhere (car a-word) (insert-everywhere a-letter (cdr a-word))))]))
