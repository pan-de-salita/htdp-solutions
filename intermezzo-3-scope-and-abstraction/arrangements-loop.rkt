#lang racket
(require test-engine/racket-tests)

;; [List-of X] -> [List-of [List-of X]]
;; creates a list of all rearrangements of the items in a-word

(define (arrangements a-word)
  (cond [(empty? a-word) '(())]
        [else (for*/list ([item a-word]
                          [arrangement-without-item
                           (arrangements (remove item a-word))])
                (cons item arrangement-without-item))]))
