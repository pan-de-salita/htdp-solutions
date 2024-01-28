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
(check-expect (extract squared>? (list 3) 10) '())
(check-expect (extract squared>? (list 4) 10) (list 4))
(check-expect (extract squared>? (list 5) 10) (list 5))
(check-expect (extract squared>? (list 3 4 5) 10) (list 4 5))

(define (extract R l t)
  (cond [(empty? l) '()]
        [else (cond [(R (car l) t) (cons (car l) (extract R (cdr l) t))]
                    [else (extract R (cdr l) t)])]))

;; Number Number -> Boolean
;; checks if the area of a square with side x is larger then a Number x

(check-expect (squared>? 3 10) #f)
(check-expect (squared>? 4 10) #t)
(check-expect (squared>? 5 10) #t)

(define (squared>? x c)
  (> (sqr x) c))

(test)

;;;; solution:

#|

(squared>? 3 10)
==
(> (sqr 3) 10)
==
(> 9 10)
== #f

(squared>? 4 10)
==
(> (sqr 4) 10)
==
(> 16 10)
==
#t

(extract squared>? (list 3 4 5) 10)
==
(cond [(empty? (list 3 4 5)) '()]
      [else (cond [(squared>? (car (list 3 4 5)) 10)
                   (cons (car (list 3 4 5)) (extract squared>? (cdr (list 3 4 5)) 10))]
                  [else (extract squared>? (cdr (list 3 4 5)) 10)])])
==
(cond [#f '()]
      [else (cond [(squared>? (car (list 3 4 5)) 10)
                   (cons (car (list 3 4 5)) (extract squared>? (cdr (list 3 4 5)) 10))]
                  [else (extract squared>? (cdr (list 3 4 5)) 10)])])
==
(cond [else (cond [(squared>? (car (list 3 4 5)) 10)
                   (cons (car (list 3 4 5)) (extract squared>? (cdr (list 3 4 5)) 10))]
                  [else (extract squared>? (cdr (list 3 4 5)) 10)])])
==
(cond [(squared>? (car (list 3 4 5)) 10)
       (cons (car (list 3 4 5)) (extract squared>? (cdr (list 3 4 5)) 10))]
      [else (extract squared>? (cdr (list 3 4 5)) 10)])
==
(cond [(squared>? 3 10)
       (cons (car (list 3 4 5)) (extract squared>? (cdr (list 3 4 5)) 10))]
      [else (extract squared>? (cdr (list 3 4 5)) 10)])
==
(cond [(> (sqr 3) 10)
       (cons (car (list 3 4 5)) (extract squared>? (cdr (list 3 4 5)) 10))]
      [else (extract squared>? (cdr (list 3 4 5)) 10)])
==
(cond [(> 9 10)
       (cons (car (list 3 4 5)) (extract squared>? (cdr (list 3 4 5)) 10))]
      [else (extract squared>? (cdr (list 3 4 5)) 10)])
==
(cond [#f
       (cons (car (list 3 4 5)) (extract squared>? (cdr (list 3 4 5)) 10))]
      [else (extract squared>? (cdr (list 3 4 5)) 10)])
==
(cond [else (extract squared>? (cdr (list 3 4 5)) 10)])
==
(extract squared>? (cdr (list 3 4 5)) 10)
==
(extract squared>? (list 4 5) 10)
==
(cond [(empty? (list 4 5)) '()]
      [else (cond [(squared>? (car (list 4 5)) 10)
                   (cons (car (list 4 5)) (extract squared>? (cdr (list 4 5)) 10))]
                  [else (extract squared>? (cdr (list 4 5)) 10)])])
==
(cond [#f '()]
      [else (cond [(squared>? (car (list 4 5)) 10)
                   (cons (car (list 4 5)) (extract squared>? (cdr (list 4 5)) 10))]
                  [else (extract squared>? (cdr (list 4 5)) 10)])])
==
(cond [else (cond [(squared>? (car (list 4 5)) 10)
                   (cons (car (list 4 5)) (extract squared>? (cdr (list 4 5)) 10))]
                  [else (extract squared>? (cdr (list 4 5)) 10)])])
==
(cond [(squared>? (car (list 4 5)) 10)
       (cons (car (list 4 5)) (extract squared>? (cdr (list 4 5)) 10))]
      [else (extract squared>? (cdr (list 4 5)) 10)])
==
(cond [(squared>? 4 10)
       (cons (car (list 4 5)) (extract squared>? (cdr (list 4 5)) 10))]
      [else (extract squared>? (cdr (list 4 5)) 10)])
==
(cond [(> (sqr 4) 10)
       (cons (car (list 4 5)) (extract squared>? (cdr (list 4 5)) 10))]
      [else (extract squared>? (cdr (list 4 5)) 10)])
==
(cond [(> 16 10)
       (cons (car (list 4 5)) (extract squared>? (cdr (list 4 5)) 10))]
      [else (extract squared>? (cdr (list 4 5)) 10)])
==
(cond [#t
       (cons (car (list 4 5)) (extract squared>? (cdr (list 4 5)) 10))]
      [else (extract squared>? (cdr (list 4 5)) 10)])
==
(cons (car (list 4 5)) (extract squared>? (cdr (list 4 5)) 10))
==
(cons 4 (extract squared>? (cdr (list 4 5)) 10))
==
(cons 4 (extract squared>? (list 5) 10))
==
(cons 4
      (cond [(empty? (list 5)) '()]
            [else (cond [(squared>? (car (list 5)) 10)
                         (cons (car (list 5)) (extract squared>? (cdr (list 5)) 10))]
                        [else (extract squared>? (cdr (list 5)) 10)])]))
==
(cons 4
      (cond [#f '()]
            [else (cond [(squared>? (car (list 5)) 10)
                         (cons (car (list 5)) (extract squared>? (cdr (list 5)) 10))]
                        [else (extract squared>? (cdr (list 5)) 10)])]))
==
(cons 4
      (cond [else (cond [(squared>? (car (list 5)) 10)
                         (cons (car (list 5)) (extract squared>? (cdr (list 5)) 10))]
                        [else (extract squared>? (cdr (list 5)) 10)])]))
==
(cons 4
      (cond [(squared>? (car (list 5)) 10)
             (cons (car (list 5)) (extract squared>? (cdr (list 5)) 10))]
            [else (extract squared>? (cdr (list 5)) 10)]))
==
(cons 4
      (cond [(squared>? 5 10)
             (cons (car (list 5)) (extract squared>? (cdr (list 5)) 10))]
            [else (extract squared>? (cdr (list 5)) 10)]))
==
(cons 4
      (cond [(> (sqr 5) 10)
             (cons (car (list 5)) (extract squared>? (cdr (list 5)) 10))]
            [else (extract squared>? (cdr (list 5)) 10)]))
==
(cons 4
      (cond [(> 25 10)
             (cons (car (list 5)) (extract squared>? (cdr (list 5)) 10))]
            [else (extract squared>? (cdr (list 5)) 10)]))
==
(cons 4
      (cond [#t
             (cons (car (list 5)) (extract squared>? (cdr (list 5)) 10))]
            [else (extract squared>? (cdr (list 5)) 10)]))
==
(cons 4
      (cons (car (list 5)) (extract squared>? (cdr (list 5)) 10)))
==
(cons 4
      (cons 5 (extract squared>? (cdr (list 5)) 10)))
==
(cons 4
      (cons 5 (extract squared>? '() 10)))
==
(cons 4
      (cons 5
              (cond [(empty? '()) '()]
                    [else (cond [(squared>? (car '()) 10)
                                 (cons (car '()) (extract squared>? (cdr '()) 10))]
                                [else (extract squared>? (cdr '()) 10)])])))
==
(cons 4
      (cons 5
              (cond [#t '()]
                    [else (cond [(squared>? (car '()) 10)
                                 (cons (car '()) (extract squared>? (cdr '()) 10))]
                                [else (extract squared>? (cdr '()) 10)])])))
==
(cons 4
      (cons 5
              '()))
==
(cons 4 (list 5))
==
(list 4 5)

|#
