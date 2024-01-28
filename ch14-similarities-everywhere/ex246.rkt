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

(extract < (cons 6 (cons 4 '())) 5)
==
(cond [(empty? (cons 6 (cons 4 '()))) '()]
      [else (cond [(< (car (cons 6 (cons 4 '()))) 5)
                   (cons (car (cons 6 (cons 4 '()))) (extract < (cdr (cons 6 (cons 4 '()))) 5))]
                  [else (extract < (cdr (cons 6 (cons 4 '()))) 5)])])
==
(cond [#f '()]
      [else (cond [(< (car (cons 6 (cons 4 '()))) 5)
                   (cons (car (cons 6 (cons 4 '()))) (extract < (cdr (cons 6 (cons 4 '()))) 5))]
                  [else (extract < (cdr (cons 6 (cons 4 '()))) 5)])])
==
(else (cond [(< (car (cons 6 (cons 4 '()))) 5)
             (cons (car (cons 6 (cons 4 '()))) (extract < (cdr (cons 6 (cons 4 '()))) 5))]
            [else (extract < (cdr (cons 6 (cons 4 '()))) 5)]))
==
(cond [(< (car (cons 6 (cons 4 '()))) 5)
       (cons (car (cons 6 (cons 4 '()))) (extract < (cdr (cons 6 (cons 4 '()))) 5))]
      [else (extract < (cdr (cons 6 (cons 4 '()))) 5)])
==
(cond [(< 6 5)
       (cons (car (cons 6 (cons 4 '()))) (extract < (cdr (cons 6 (cons 4 '()))) 5))]
      [else (extract < (cdr (cons 6 (cons 4 '()))) 5)])
==
(cond [#f
       (cons (car (cons 6 (cons 4 '()))) (extract < (cdr (cons 6 (cons 4 '()))) 5))]
      [else (extract < (cdr (cons 6 (cons 4 '()))) 5)])
==
(cond [else (extract < (cdr (cons 6 (cons 4 '()))) 5)])
==
(extract < (cdr (cons 6 (cons 4 '()))) 5)
==
(extract < (cons 4 '()) 5)

|#
