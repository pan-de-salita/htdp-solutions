#lang racket
(require test-engine/racket-tests)

;; [List-of Number] [Number Number -> Boolean] -> [List-of Number]
;; produces a version of l-num, sorted according to cmp

(check-expect (sort-cmp '() >) '())
(check-expect (sort-cmp (list 1 2) >) (list 2 1))
(check-expect (sort-cmp (list 12 43 5 328) <) (list 5 12 43 328))

(define (sort-cmp l-num cmp)
  (local (;; [List-of Number] -> [List-of Number]
          ;; produces a sorted l-num
          (define (isort l-num-to-sort)
            (cond [(empty? l-num-to-sort) '()]
                  [else (insert (car l-num-to-sort)
                                (isort (cdr l-num-to-sort)))]))
          ;; Number [List-of Number] -> [List-of Number]
          ;; inserts num into a sorted l-num
          (define (insert num l-num-sorted)
            (cond [(empty? l-num-sorted) (list num)]
                  [else (cond [(cmp num (car l-num-sorted))
                               (cons num l-num-sorted)]
                              [else (cons (car l-num-sorted)
                                          (insert num (cdr l-num-sorted)))])])))
    (isort l-num)))

(test)
