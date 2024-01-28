#lang racket
(require test-engine/racket-tests)

;;;; function used for solution:

;; Comparator List-of-Numbers Number -> Number
;; returns those numbers on l that meet the condition(s) of the
;; Comparator in respect to t.

(check-expect (extract < '() 2) '())
(check-expect (extract < '(0 1 2) 2) '(0 1))
(check-expect (extract <= '(0 1 2) 2) '(0 1 2))
(check-expect (extract > '(0 1 2) 2) '())
(check-expect (extract >= '(0 1 2) 2) '(2))

(define (extract R l t)
  (cond [(empty? l) '()]
        [else (cond [(R (car l) t) (cons (car l) (extract R (cdr l) t))]
                    [else (extract R (cdr l) t)])]))

(test)

;;;; solution:

#|

(extract < (cons 8 (cons 4 '())) 5)
==
(cond [(empty? (cons 8 (cons 4 '()))) '()]
      [else (cond [(< (car (cons 8 (cons 4 '()))) 5)
                   (cons (car (cons 8 (cons 4 '()))) (extract < (cdr (cons 8 (cons 4 '()))) 5))]
                  [else (extract < (cdr (cons 8 (cons 4 '()))) 5)])])
==
(cond [#f '()]
      [else (cond [(< (car (cons 8 (cons 4 '()))) 5)
                   (cons (car (cons 8 (cons 4 '()))) (extract < (cdr (cons 8 (cons 4 '()))) 5))]
                  [else (extract < (cdr (cons 8 (cons 4 '()))) 5)])])
==
(cond [else (cond [(< (car (cons 8 (cons 4 '()))) 5)
                   (cons (car (cons 8 (cons 4 '()))) (extract < (cdr (cons 8 (cons 4 '()))) 5))]
                  [else (extract < (cdr (cons 8 (cons 4 '()))) 5)])])
==
(cond [(< (car (cons 8 (cons 4 '()))) 5)
       (cons (car (cons 8 (cons 4 '()))) (extract < (cdr (cons 8 (cons 4 '()))) 5))]
      [else (extract < (cdr (cons 8 (cons 4 '()))) 5)])
==
(cond [(< 8 5)
       (cons (car (cons 8 (cons 4 '()))) (extract < (cdr (cons 8 (cons 4 '()))) 5))]
      [else (extract < (cdr (cons 8 (cons 4 '()))) 5)])
==
(cond [#f
       (cons (car (cons 8 (cons 4 '()))) (extract < (cdr (cons 8 (cons 4 '()))) 5))]
      [else (extract < (cdr (cons 8 (cons 4 '()))) 5)])
==
(cond [else (extract < (cdr (cons 8 (cons 4 '()))) 5)])
==
(extract < (cdr (cons 8 (cons 4 '()))) 5)
==
(extract < (cons 4 '()) 5)
==
(cond [(empty? (cons 4 '())) '()]
      [else (cond [(< (car (cons 4 '())) 5)
                   (cons (car (cons 4 '())) (extract < (cdr (cons 4 '())) 5))]
                  [else (extract < (cdr (cons 4 '())) 5)])])
==
(cond [#f '()]
      [else (cond [(< (car (cons 4 '())) 5)
                   (cons (car (cons 4 '())) (extract < (cdr (cons 4 '())) 5))]
                  [else (extract < (cdr (cons 4 '())) 5)])])
==
(cond [else (cond [(< (car (cons 4 '())) 5)
                   (cons (car (cons 4 '())) (extract < (cdr (cons 4 '())) 5))]
                  [else (extract < (cdr (cons 4 '())) 5)])])
==
(cond [(< (car (cons 4 '())) 5)
       (cons (car (cons 4 '())) (extract < (cdr (cons 4 '())) 5))]
      [else (extract < (cdr (cons 4 '())) 5)])
==
(cond [(< 4 5)
       (cons (car (cons 4 '())) (extract < (cdr (cons 4 '())) 5))]
      [else (extract < (cdr (cons 4 '())) 5)])
==
(cond [#t
       (cons (car (cons 4 '())) (extract < (cdr (cons 4 '())) 5))]
      [else (extract < (cdr (cons 4 '())) 5)])
==
(cons (car (cons 4 '())) (extract < (cdr (cons 4 '())) 5))
==
(cons 4 (extract < (cdr '()) 5))
==
(cons 4 (extract < '() 5))
==
(cons 4
      (cond [(empty? '()) '()]
            [else (cond [(< (car '()) 5) (cons (car '()) (extract < (cdr '()) 5))]
                        [else (cons (car '()) (extract < (cdr '()) 5))])]))
==
(cons 4
      (cond [#t '()]
            [else (cond [(< (car '()) 5) (cons (car '()) (extract < (cdr '()) 5))]
                        [else (cons (car '()) (extract < (cdr '()) 5))])]))
==
(cons 4 '())

|#
