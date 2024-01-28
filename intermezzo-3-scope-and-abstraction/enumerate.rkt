#lang racket
(require test-engine/racket-tests)

;; [List-of X] -> [List-of [List-of N X]]
;; pairs each item in lx with its index

(check-expect (enumerate '()) '())
(check-expect (enumerate '(1)) '((0 1)))
(check-expect (enumerate '(1 2)) '((0 1) (1 2)))
(check-expect (enumerate '(1 2 3)) '((0 1) (1 2) (2 3)))
(check-expect (enumerate '("a" "b" "c")) '((0 "a") (1 "b") (2 "c")))

(define (enumerate lx)
  (for/list ([index (length lx)]
             [x lx])
    (list index x)))

;; [List-of X] -> [List-of [List-of N X]]
;; like enumerate, pairs each item in lx with its index

(check-expect (enumerate.func '()) (enumerate '()))
(check-expect (enumerate.func '(1)) (enumerate '(1)))
(check-expect (enumerate.func '(1 2)) (enumerate '(1 2)))
(check-expect (enumerate.func '(1 2 3)) (enumerate '(1 2 3)))
(check-expect (enumerate.func '("a" "b" "c")) (enumerate '("a" "b" "c")))

(define (enumerate.func lx)
  (reverse
   (foldl (lambda (x l-occurring)
            (cons (list (length l-occurring) x)
                  l-occurring))
          '()
          lx)))

(test)
