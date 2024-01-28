#lang racket
(require test-engine/racket-tests)

;; Number -> [List-of [List-of Number]]
;; returns an indentity matrix of size (* n n)

(check-expect (identity-M 0) (list '()))
(check-expect (identity-M 1) (list (list 1)))
(check-expect (identity-M 3) (list (list 1 0 0) (list 0 1 0) (list 0 0 1)))

(define (identity-M n)
  (cond [(= n 0) (list '())]
        [else (local (;; 1. create a list of 0s of size (sub1 n)
                      (define list-of-0s (make-list (sub1 n) 0))
                      ;; 2. insert 1 into all possible positions within list-of-0s
                      ;; [List-of Number] -> [List-of [List-of Number]]
                      (define (insert-1-everywhere l-x)
                        (cond [(empty? l-x) (list (list 1))]
                              [else (local ((define inserted-1-everywhere/cdr (insert-1-everywhere (cdr l-x)))
                                            ;; [List-of [List-of X]] -> [List-of [List-of X]]
                                            ;; prepends 0 to all items within a list of list of X
                                            (define (prepend-0-to l-x) (cons 0 l-x)))
                                      (cons (cons 1 l-x)
                                            (map prepend-0-to inserted-1-everywhere/cdr)))])))
                (insert-1-everywhere list-of-0s))]))

(test)

#|

refresher on insert-everywhere function (rewritten into insert-1-everywhere above):

(insert-everywhere 1 (list 0 0))
==
(cond [(empty? (list 0 0)) (list (list 1))]
      [else (cons (cons 1 (list 0 0))
                  (prepend-everywhere (car (list 0 0))
                                      (insert-everyere 1 (cdr (list 0 0)))))])
==
(cond [#f (list (list 1))]
      [else (cons (cons 1 (list 0 0))
                  (prepend-everywhere (car (list 0 0))
                                      (insert-everyere 1 (cdr (list 0 0)))))])
==
(cond [else (cons (cons 1 (list 0 0))
                  (prepend-everywhere (car (list 0 0))
                                      (insert-everyere 1 (cdr (list 0 0)))))])
==
(cons (cons 1 (list 0 0))
      (prepend-everywhere (car (list 0 0))
                          (insert-everyere 1 (cdr (list 0 0)))))
==
(cons (list 1 0 0)
      (prepend-everywhere (car (list 0 0))
                          (insert-everyere 1 (cdr (list 0 0)))))
==
(cons (list 1 0 0)
      (prepend-everywhere 0
                          (insert-everyere 1 (cdr (list 0 0)))))
==
(cons (list 1 0 0)
      (prepend-everywhere 0
                          (insert-everyere 1 (list 0))))
==
(cons (list 1 0 0)
      (prepend-everywhere 0
                          (cond [(empty? (list 0)) (list (list 1))]
                                [else (cons (cons 1 (list 0))
                                            (prepend-everywhere (car (list 0))
                                                                (insert-everywhere 1 (cdr (list 0)))))])))
==
(cons (list 1 0 0)
      (prepend-everywhere 0
                          (cond [#f (list (list 1))]
                                [else (cons (cons 1 (list 0))
                                            (prepend-everywhere (car (list 0))
                                                                (insert-everywhere 1 (cdr (list 0)))))])))
==
(cons (list 1 0 0)
      (prepend-everywhere 0
                          (cond [else (cons (cons 1 (list 0))
                                            (prepend-everywhere (car (list 0))
                                                                (insert-everywhere 1 (cdr (list 0)))))])))
==
(cons (list 1 0 0)
      (prepend-everywhere 0
                          (cons (cons 1 (list 0))
                                (prepend-everywhere (car (list 0))
                                                    (insert-everywhere 1 (cdr (list 0)))))))
==
(cons (list 1 0 0)
      (prepend-everywhere 0
                          (cons (list 1 0)
                                (prepend-everywhere (car (list 0))
                                                    (insert-everywhere 1 (cdr (list 0)))))))
==
(cons (list 1 0 0)
      (prepend-everywhere 0
                          (cons (list 1 0)
                                (prepend-everywhere 0
                                                    (insert-everywhere 1 (cdr (list 0)))))))
==
(cons (list 1 0 0)
      (prepend-everywhere 0
                          (cons (list 1 0)
                                (prepend-everywhere 0
                                                    (insert-everywhere 1 '())))))
==
(cons (list 1 0 0)
      (prepend-everywhere 0
                          (cons (list 1 0)
                                (prepend-everywhere 0
                                                    (cond [(empty? '()) (list (list 1))]
                                                          [else (cons (cons 1 '())
                                                                      (prepend-everywhere (car '())
                                                                                          (insert-everywhere 1 (cdr '()))))])))))
==
(cons (list 1 0 0)
      (prepend-everywhere 0
                          (cons (list 1 0)
                                (prepend-everywhere 0
                                                    (cond [#t (list (list 1))]
                                                          [else (cons (cons 1 '())
                                                                      (prepend-everywhere (car '())
                                                                                          (insert-everywhere 1 (cdr '()))))])))))
==
(cons (list 1 0 0)
      (prepend-everywhere 0
                          (cons (list 1 0)
                                (prepend-everywhere 0
                                                    (list (list 1))))))
(cons (list 1 0 0)
      (prepend-everywhere 0
                          (cons (list 1 0)
                                (cond [(empty? (list (list 1))) '()]
                                      [else (cons (cons 0 (car (list (list 1))))
                                                  (prepend-everywhere 0 (cdr (list (list 1)))))]))))
==
(cons (list 1 0 0)
      (prepend-everywhere 0
                          (cons (list 1 0)
                                (cond [#f '()]
                                      [else (cons (cons 0 (car (list (list 1))))
                                                  (prepend-everywhere 0 (cdr (list (list 1)))))]))))
==
(cons (list 1 0 0)
      (prepend-everywhere 0
                          (cons (list 1 0)
                                (cond [else (cons (cons 0 (car (list (list 1))))
                                                  (prepend-everywhere 0 (cdr (list (list 1)))))]))))
==
(cons (list 1 0 0)
      (prepend-everywhere 0
                          (cons (list 1 0)
                                (cons (cons 0 (car (list (list 1))))
                                      (prepend-everywhere 0 (cdr (list (list 1)))))))))
==
(cons (list 1 0 0)
      (prepend-everywhere 0
                          (cons (list 1 0)
                                (cons (cons 0 (list 1))
                                      (prepend-everywhere 0 (cdr (list (list 1))))))))
==
(cons (list 1 0 0)
      (prepend-everywhere 0
                          (cons (list 1 0)
                                (cons (list 0 1)
                                      (prepend-everywhere 0 (cdr (list (list 1))))))))
==
(cons (list 1 0 0)
      (prepend-everywhere 0
                          (cons (list 1 0)
                                (cons (list 0 1)
                                      (prepend-everywhere 0 '())))))
==
(cons (list 1 0 0)
      (prepend-everywhere 0
                          (cons (list 1 0)
                                (cons (list 0 1)
                                      (cond [(empty? '()) '()]
                                            [else (cons (cons 0 (car '()))
                                                        (prepend-everywhere 0 (cdr '())))])))))
==
(cons (list 1 0 0)
      (prepend-everywhere 0
                          (cons (list 1 0)
                                (cons (list 0 1)
                                      (cond [#t '()]
                                            [else (cons (cons 0 (car '()))
                                                        (prepend-everywhere 0 (cdr '())))])))))
==
(cons (list 1 0 0)
      (prepend-everywhere 0
                          (cons (list 1 0)
                                (cons (list 0 1)
                                      '()))))
==
(cons (list 1 0 0)
      (prepend-everywhere 0
                          (cons (list 1 0)
                                (list (list 0 1)))))
==
(cons (list 1 0 0)
      (prepend-everywhere 0
                          (list (list 1 0) (list 0 1))))
==
(cons (list 1 0 0)
      (cond [(empty? (list (list 1 0) (list 0 1))) '()]
            [else (cons (cons 0 (car (list (list 1 0) (list 0 1))))
                        (insert-everywhere 0 (cdr (list (list 1 0) (list 0 1)))))]))
==
(cons (list 1 0 0)
      (cond [#f '()]
            [else (cons (cons 0 (car (list (list 1 0) (list 0 1))))
                        (insert-everywhere 0 (cdr (list (list 1 0) (list 0 1)))))]))
==
(cons (list 1 0 0)
      (cond [else (cons (cons 0 (car (list (list 1 0) (list 0 1))))
                        (insert-everywhere 0 (cdr (list (list 1 0) (list 0 1)))))]))
==
(cons (list 1 0 0)
      (cons (cons 0 (car (list (list 1 0) (list 0 1))))
            (insert-everywhere 0 (cdr (list (list 1 0) (list 0 1))))))
==
(cons (list 1 0 0)
      (cons (cons 0 (list 1 0))
            (insert-everywhere 0 (cdr (list (list 1 0) (list 0 1))))))
==
(cons (list 1 0 0)
      (cons (list 0 1 0)
            (insert-everywhere 0 (cdr (list (list 1 0) (list 0 1))))))
==
(cons (list 1 0 0)
      (cons (list 0 1 0)
            (insert-everywhere 0 (list (list 0 1)))))
==
(cons (list 1 0 0)
      (cons (list 0 1 0)
            (cond [(empty? (list (list 0 1))) '()]
                  [else (cons (cons 0 (car (list (list 0 1))))
                              (insert-everywhere 0 (cdr (list (list 0 1)))))])))
==
(cons (list 1 0 0)
      (cons (list 0 1 0)
            (cond [#f '()]
                  [else (cons (cons 0 (car (list (list 0 1))))
                              (insert-everywhere 0 (cdr (list (list 0 1)))))])))
==
(cons (list 1 0 0)
      (cons (list 0 1 0)
            (cond [else (cons (cons 0 (car (list (list 0 1))))
                              (insert-everywhere 0 (cdr (list (list 0 1)))))])))
==
(cons (list 1 0 0)
      (cons (list 0 1 0)
            (cons (cons 0 (car (list (list 0 1))))
                  (insert-everywhere 0 (cdr (list (list 0 1)))))))
==
(cons (list 1 0 0)
      (cons (list 0 1 0)
            (cons (cons 0 (list 0 1))
                  (insert-everywhere 0 (cdr (list (list 0 1)))))))
==
(cons (list 1 0 0)
      (cons (list 0 1 0)
            (cons (list 0 0 1)
                  (insert-everywhere 0 (cdr (list (list 0 1)))))))
==
(cons (list 1 0 0)
      (cons (list 0 1 0)
            (cons (list 0 0 1)
                  (insert-everywhere 0 '()))))
==
(cons (list 1 0 0)
      (cons (list 0 1 0)
            (cons (list 0 0 1)
                  (cond [(empty? '()) '()]
                        [else (cons (cons 0 (car '()))
                                    (insert-everywhere 0 (cdr '())))]))))
==
(cons (list 1 0 0)
      (cons (list 0 1 0)
            (cons (list 0 0 1)
                  (cond [#t '()]
                        [else (cons (cons 0 (car '()))
                                    (insert-everywhere 0 (cdr '())))]))))
==
(cons (list 1 0 0)
      (cons (list 0 1 0)
            (cons (list 0 0 1)
                  '())))
==
(cons (list 1 0 0)
      (cons (list 0 1 0)
            (list (list 0 0 1))))
==
(cons (list 1 0 0)
      (list (list 0 1 0) (list 0 0 1)))
==
(list (list 1 0 0) (list 0 1 0) (list 0 0 1))


|#
