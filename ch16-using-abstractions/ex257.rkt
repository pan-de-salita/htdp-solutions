#lang racket
(require test-engine/racket-tests)

;; [X Y] [X Y -> Y] Y [List-of X] -> Y
;; f*oldl works just like foldl

(check-expect (f*oldl + 0 (list 0 1 2)) (foldl + 0 (list 0 1 2)))
(check-expect (f*oldl - 1 (list 0 1 2)) (foldl - 1 (list 0 1 2)))

(define (f*oldl f e l)
  (foldr f e (reverse l)))

;; [X] N [N -> X] -> [List-of X]
;; build-l*st works just like build-list

(check-expect (build-l*st 3 add1) (build-list 3 add1))

(define (build-l*st n f)
  (cond [(= n 0) '()]
        [else (add-at-end (f (sub1 n)) (build-l*st (sub1 n) f))]))

;; X [List-of X] -> [List-of X]
;; adds X to the end of a list of X

(check-expect (add-at-end 0 '()) (list 0))
(check-expect (add-at-end 3 (list 1 2)) (list 1 2 3))

(define (add-at-end x l-x)
  (cond [(empty? l-x) (list x)]
        [else (cons (car l-x) (add-at-end x (cdr l-x)))]))

(test)
