#lang racket
(require test-engine/racket-tests)

;; List-of-String String -> N
;; determines how often s occurs in los
(check-expect (count (cons "cat" (cons "dog" (cons "cat" '()))) "cat") 2)
(check-expect (count (cons "cat" (cons "dog" (cons "cat" '()))) "bread") 0)

(define (count los s)
  (cond [(empty? los) 0]
        [(cons? los)
         (if (string=? (first los) s)
             (add1 (count (rest los) s))
             (count (rest los) s))]))

(test)
