#lang racket
(require test-engine/racket-tests)

;; [X -> Y] [List-of X] -> [List-of Y]
;; a simulation of map using foldr

(check-expect (map-via-fold add1 '(1 2 3))
              (map add1 '(1 2 3)))
(check-expect (map-via-fold number->string '(1 2 3))
              (map number->string '(1 2 3)))

(define (map-via-fold F l-x)
  (foldr (lambda (x constructed-list)
           (cons (F x) constructed-list))
         '()
         l-x))

;; NOTE: using foldl effectively reverses the resulting list:
;; [X -> Y] [List-of X] -> [List-of Y]
;; a simulation of reverse applied to map using foldl

(check-expect (map-via-fold/reverse add1 '(1 2 3))
              (reverse (map-via-fold add1 '(1 2 3))))
(check-expect (map-via-fold/reverse number->string '(1 2 3))
              (reverse (map-via-fold number->string '(1 2 3))))

(define (map-via-fold/reverse F l-x)
  (foldl (lambda (x constructed-list)
           (cons (F x) constructed-list))
         '()
         l-x))

(test)
